(ns no.olavfosse.grep
  (import java.util.LinkedList))

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
 (into []
       (-B 2 #(= % :HERE))
       [:HERE "fdjkasl" "fkla" "jfka" "jkfdla" :HERE :HERE "jfkdlas" "kjlfa" "jkflda" :HERE]) := [:HERE "jfka" "jkfdla" :HERE :HERE "kjlfa" "jkflda" :HERE]
 ;; To test that we handle reduced values properly - which we do
 (defn ffff [xform]
   (transduce xform
              (completing (fn [acc inp]
                            (if (= inp :REDUCED)
                              (reduced acc)
                              (conj acc inp))))
              []
              [:buzz :fizz :boo :REDUCED :baa ]))
 (ffff (-B 10000 (constantly true))) := (ffff (map identity))
 
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
