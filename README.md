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
