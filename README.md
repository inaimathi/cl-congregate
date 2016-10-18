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

- /me (show all the meetups/events this user moderates and has attended)
- Group should have some starting-in notation so that you could set up a group well in advance of its first meeting
- Email notifications
- Edit event page
- Markdown-friendly description field (this implies no styling on general heading tags; use classes for congregate-structure stuff instead)
- Create groups
- Time-zone sensitivity for the basic infrastructure
- Comment system
- RSS feed per country/city/location/group/event/user
- API for viewing/editing events and groups
