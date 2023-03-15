# Rustscript Syntax and its javascript/typescript equivalent

## Returns without `return`

```
fn foo() {
    "bar"
}
```

compiles to this javascript:

```javascript
function foo() {
    return "bar";
}
```

---

## Pattern matching? and Option<things>

```
fn test(in: Option<boolean>) {
    let stuff = if let Some(v) = in {
        v
    } else {
        "Hello"
    };

    stuff
}
```

compiles to this javascript:

```javascript
function test(in) {
    let stuff;
    if (in != null) {
        stuff = in;
    } else {
        stuff = "Hello";
    }
    return stuff;
}

```

or this javascript:

```javascript
function test(in) {
    let stuff = in != null ? in : "Hello";
    return stuff;
}

```

---

## Match

```
fn matchStuff(in: Option<string>) {
    match in {
        Some(v) => console.log(v),
        None => console.error("Hello")
    }
}
```

compiles to this javascript:

```javascript
function matchStuff(in) {
    switch (in) {
        case null:
            console.error("Hello");
            break;
        default:
            console.log(in);
    }
}

```

---

# Macros

how would this work?