(ns no.olavfosse.grep
  (:require [hyperfiddle.rcf :refer [tests]])
  (:import java.util.LinkedList))

(hyperfiddle.rcf/enable!)

(def ^:private test-transactions
  [{:status :success
    :product :chocolate}
   {:status :success
    :product :kebab}
   {:status :success
    :product :burger}
   {:status :success
    :product :energy-drink}
   {:status :success
    :product :chicken}
   {:status :failure
    :product :bus-ticket}
   {:status :failure
    :product :wolt-bike}
   {:status :failure
    :product :umbrella}

   ;; Next day
   {:status :success
    :product :second-hand-bike}

   ;; Friday
   {:status :success
    :time "2024-08-01T17:21:36"
    :product :cider}
   {:status :success
    :time "2024-08-01T17:31:36"
    :product :mango-ipa}
   {:status :success
    :time "2024-08-01T17:41:36"
    :product :pils}
   {:status :success
    :time "2024-08-01T17:51:36"
    :product :pils}
   {:status :success
    :time "2024-08-01T17:51:36"
    :product :kebab}])

(defn -B "Returns a stateful transducer akin to $ grep -B n <pred>"
  [n pred]
  (fn [rf]
    (let [context-trail (java.util.LinkedList.)]
      (fn
        ([] (rf))
        ([acc] (rf acc))
        ([acc inp]
         (if-not (pred inp)
           (do (.addFirst context-trail inp)
               (when (= (.size context-trail) (inc n)) (.removeLast context-trail))
               acc)
           (loop [acc acc]
             (if-some [trail-inp (.pollLast context-trail)] ;;
               (let [rv (rf acc trail-inp)]
                 (if (reduced? rv) rv (recur rv)))
               (rf acc inp)))))))))
(tests
  ;; What lead up to not being able to afford the bus ticket?
  (into [] (-B 3 (fn [{:keys [status product]}] (= [status product] [:failure :bus-ticket]))) test-transactions)
  := [{:status :success
       :product :burger}
      {:status :success
       :product :energy-drink}
      {:status :success
       :product :chicken}
      {:status :failure
       :product :bus-ticket}]

  ;; What happened right before and after the failing to purchase a bus ticket?
  (into [] (-C 1 (fn [{:keys [status product]}] (= [status product] [:failure :bus-ticket]))) test-transactions)
  := [{:status :success
       :product :chicken}
      {:status :failure
       :product :bus-ticket}
      {:status :failure
       :product :wolt-bike}]
  )

(defn -A "Returns a stateful transducer akin to $ grep -A n <pred>"
  [n pred]
  (fn [rf]
    (let [!inputs-since-match (volatile! ##Inf)]
      (fn
        ([] (rf))
        ([acc] (rf acc))
        ([acc inp]
         (vswap! !inputs-since-match inc)
         (cond
           (pred inp) (do (vreset! !inputs-since-match 0)
                          (rf acc inp))
           (<= @!inputs-since-match n) (rf acc inp)
           :else acc))))))
(tests
 (into []
       (-A 3 #(= % :foo))
       [:a :b :c :foo 1 2 3 4]) := [:foo 1 2 3])

(defn -C "Returns a stateful transducer akin to $ grep -C n <pred>"
  [n pred]
  (fn [rf]
    (let [;; context-trail is the trail of inputs which should be
          ;; forwarded if the next input matches the predicate. The
          ;; trail must not contain inputs which have already been
          ;; forwarded.
          context-trail (java.util.LinkedList.)
          ;; !inputs-since-match is the number of inputs forwarded
          ;; since the last match.
          !inputs-since-match (volatile! ##Inf)]
      (fn
        ([] (rf))
        ([acc] (rf acc))
        ([acc inp]
         (if (pred inp)
           (do
             (vreset! !inputs-since-match 0)
             (loop [acc acc]
               (if-some [trail-inp (.pollLast context-trail)]
                 (let [rv (rf acc trail-inp)]
                   (if (reduced? rv) rv (recur rv)))
                 (rf acc inp))))
           (if (< @!inputs-since-match n)
             (do (vswap! !inputs-since-match inc)
                 (rf acc inp))
             (do 
               (.addFirst context-trail inp)
               (when (= (.size context-trail) (inc n)) (.removeLast context-trail))
               acc))))))))
(tests
 (into [] (-C 5 keyword?) [:c "a" "b" "c" :c "a"]))
