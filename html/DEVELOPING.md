# Running the MELPA website locally for development

First, grab a copy of the JSON files from the live site for local use:

    make livejson

Then, run a local webserver, e.g. `python -m SimpleHTTPServer` or
`liveserver` (from `npm`).

Now you should be able to visit the locally-served copy of the MELPA
website and tweak the HTML/Javascript to your liking.
