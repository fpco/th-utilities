* Add utilities for providing proxy to methods of deriver

* Add utilities for taking references to a deriver's methods and turning them
  into local references. Probably have the same utility wrap things up in a let
  or make top level decls.  Wish TH could make hidden top level decls!

* Implement convenient ways to define Derivers / Instantiators.  See commented
  out code in "TH.Derive" and also code in my old instance-templates project.
