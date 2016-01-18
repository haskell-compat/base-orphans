## Changes in 0
* This is an intentional empty release. `base-orphans-0` is used when
  retroactively adding `base-orphans` dependencies to older Hackage libraries
  to ensure that they cannot be built in combination with more recent versions
  of `base-orphans` that would cause them to break. (For example, if a package
  defines an orphan instance which clashes with one in `base-orphans`.)
