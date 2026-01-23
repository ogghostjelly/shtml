# SHTML

HTML at Compile-time

> [!NOTE]
> I am planning to rebrand SHTML because it conflicts with the name of SSI `.shtml` files.
> 
> This project is still in active (albeit slow) development so expect breaking changes.
> The documentation isn't great either but it's something I'll be working on.
> In the meantime feel free to contribute!

A lisp-like HTML templating language built off of [mal](https://github.com/kanaka/mal).
This is not suited for major production settings.
If you found a bug or have a suggestion [create an issue](https://github.com/OgGhostJelly/shtml/issues/new) or to submit code [create a pull request](https://github.com/OgGhostJelly/shtml/compare).
I made this because I was dissatisfied with the state of <abbr title="Static Site Generator">SSG</abbr> templating languages.

Clone and run it:
```bash
git clone https://github.com/OgGhostJelly/shtml.git
cd shtml
cargo run -- help         # Display help text
#cargo run -- build site/ # Build the example site
#cargo run -- repl        # Start the repl
```

# Examples

## Numbers, comments and ignoring
```html
<main>
    @; this is a comment
    @(+ 1 2)
    
    @; '#' ignores the value
    @(# + 5 6)
</main>

--- vvv ---

<main>
    3


</main>
```

## Using more complex functions
```html
<main>
    @(do
        (def! r (range 0 10))
        (str (join r " + ") " = " (accum + r)))
</main>

--- vvv ---

<main>
    0 + 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 = 45
</main>
```

## Including files
```html
[index.html]
<main>
    @(include "#template.html" [greeting "Hello"])
</main>

[#template.html]
<p>@greeting from @file</p>

--- vvv ---

<main>
    <p>Hello from #template.html</p>
</main>
```

## Custom tags
```html
<main>
    @(defn! x@say
        [p text]
        <p>"@text" said @(p 'name)</p>)

    <x@say name="Bob">What's up</x@say>
</main>

--- vvv ---

<main>
    <p>"What's up" said Bob</p>
</main>
```

## Compiling to JS
> [!IMPORTANT]
> This is not yet implemented, there are older versions of shtml that do implement this but they are very buggy and janky!
```html
<main>
    @(def! greeting "Hello, World!")
    @(script (alert ~greeting))
</main>

--- vvv ---

<main>
    <script> alert("Hello, World") </script>
</main>
```