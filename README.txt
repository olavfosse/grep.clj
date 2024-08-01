**UNDER CONSTRUCTION**

I'm working on a little library for bringing the niceties of grep out
of the dark sluggish chambers of the unix realm.

I haven't quite decided on the API yet and I need to test this a bit
to make sure I covered all the edge cases.

**Design**
* Should the output be context windows?
+ grep uses context windows
+ flattening is trivial and elegant (comp (-C n pred) cat), the reverse
  is nontrivial
+ having separator items sucks
- context windows is inherently less real time, as you cant release the
  window until you've closed it
- for time series data, i.e logs, windowing is unesecary

* Should there be a way to 
