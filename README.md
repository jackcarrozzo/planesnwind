# planesnwind 

_I wish I had thought of a cooler name for this before writing a jillion lines
that depend on the package name, but it is at least descriptive._

**This code optimizes flight paths by being wind-aware.**

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

Because both fetching from the FlightAware API (or scraping the web view) and
applying the genetic algorithm path improvements are resource intensive, the code
uses a worker pool coordinated by Postgres. Due to the nature of the problem,
computation shards extremely well (by flight), and thus can be scaled infinitely.

The example data was generated over several days, running in real-time on a cluster
of 45 machines, fetching and optimizing weather and flights in real time for the 
entire continental US.

**The mean flight fuel saving on a given day for ALL continental flights is 
between 11% and 18%! On windy days, many flights are improved by 30% or more.**

![Example screenshot of web ui](/static/screenshot/example1.png)

The `static/` directory includes a simple web ui to view the output; in addition to
metrics about individual flights in the data table, you can compare the original flight
position log with (1) an openly optimized path based on the winds during that flight and
(2) a flight path optimized to take advantage of winds, but constrained to not be any
shorter (distance-wise) than the original track. The latter helps prove how much
fuel is saved from purely wind advantages vs a track that simply "got shorter".
