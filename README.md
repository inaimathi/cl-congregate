# cl-congregate

_An open-source event coordination service_

## Usage

1. Install `quicklisp`, along with `house`
2. Clone this repo
3. Add a `secrets.lisp` file that declares into the `cl-congregate` package the variables
	- `+google-api-key+`
	- `+github-api-id+`
	- `+github-api-secret+`
4. In a Lisp REPL, evaluate`(ql:quickload :cl-congregate)` followed by `(cl-congregate:start 8000)`
5. Browse to `http://localhost:8000`

## TODO

- Attendance model and interface
- House session improvements and real subdomain handling
	- you need to update how the session handles cookies. Specifically, it should be possible to specify domains for sessions, and take said domains as a top-level keyword arg to `start`. That way you could set the same session token for `congregate.ca`, `code-retreat.congregate.ca` and `congregate.inaimathi.ca/code-retreat`.
- Edit event page
- House routing improvements
	- you need to update how House stores handlers and routes to them. Specifically, copy over the `cl-handlers` machinery for handling path component arguments with type annotations. This would make it possible to make one handler for `/:group::group` (probably safer to do `/g/:group::group` or something just so it's not off of root) instead of separate ones for each individual group
	- Also, focus on making `cl-handlers` and `house` notation-compatible for the purposes of `define-handler`, HTTP type definitions and annotations, that way we can port between them later.
- Markdown-friendly description field (this implies no styling on general heading tags; use classes for congregate-structure stuff instead)- Markdown-friendly description field (this implies no styling on general heading tags; use classes for congregate-structure stuff instead)
- RSS-feed per group
- iCal feed per group
- House performance improvements (assuming you haven't moved to `:woo` by this point)
- Email notifications
- Group should have some starting-in notation so that you could set up a group well in advance of its first meeting
- Create groups
- Time-zone sensitivity for the basic infrastructure
- Comment system
- RSS feed per country/city/location/group/event/user
- API for viewing/editing events and groups
