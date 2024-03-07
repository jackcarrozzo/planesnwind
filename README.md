# planesnwind 

_I wish I had thought of a cooler name for this before writing a jillion lines
that depend on the package name, but it is at least descriptive._

** This code optimizes flight paths by being wind-aware. **

Commercially registered
"flights" (ie, "United 333" etc) are not just src-dest pairs but also mostly-static*
routes between those pts. If these routes were dynamically generated right before departure 
to take advantage of known wind conditions, much fuel and time can be saved.

\* Air traffic controllers, workload and traffic load permitting, are able to verbally
adjust a flight's route in their area, but this is ad hoc and rarely more than a few
% overall improvement.

### Simplified actions:

- Fetch flight tracks across continental US since last worker invocation
- Fetch wind data at altitude and ground across continental US
- Build 3d structured grid model of airmass movement
- "Fly" planes' position data through airmass to measure correctness
- Use genetic algorithms to adjust flight path to use less fuel / less flight time
- Produce cool maps of optimized paths and sum metrics

** The mean flight fuel saving on a given day for ALL continental flights is 
between 11% and 18%! On windy days, many flights are improved by 30% or more. **
