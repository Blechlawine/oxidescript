compile-examples:
    for word in ./examples/**/*.os; do cargo run -- --input $word compile && mv ./main.js $(dirname $word)/main.js; done
