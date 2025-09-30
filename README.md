# SHTML

HTML at Compile-time

> [!NOTE]
> This project is still in active (albeit slow) development, so expect breaking changes.

A lisp-like HTML templating language built off of [mal](https://github.com/kanaka/mal).
This is not suited for major production settings.
If you found a bug or have a suggestion [create an issue](https://github.com/OgGhostJelly/shtml/issues/new) or to submit code [create a pull request](https://github.com/OgGhostJelly/shtml/compare).
I made this because I was dissatisfied with the state of <abbr title="Static Site Generator">SSG</abbr> templating languages.

Clone and run it:
```bash
git clone https://github.com/OgGhostJelly/shtml.git
cd shtml
cargo run -- build site/ # Build the example site
```

# Examples

Numbers, comments and ignoring:
```html
<main>
    @(; this is a comment)
    @(+ 1 2)
    
    @(; '#' ignores the value)
    @(# + 5 6)
</main>

--- vvv ---

<main>
    3


</main>
```

Using more complex functions:
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

Including files and compiling to JS:
> [!WARNING]
> This is not yet implemented, there are older versions of shtml that do implement this but they are very buggy and janky!
```html
[index.html]
<main>
    @(include "#template.html" [name "World" mood "Good"])
</main>

[#template.html]
@(; This will get compiled to JS)
@(script (alert ~name))

<p>Hello @name feeling @mood today?</p>

--- vvv ---

<main>
    <script> alert("World") </script>
    <p>Hello World feeling Good today?<p>
</main>
```