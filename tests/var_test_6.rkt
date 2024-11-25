(let ([x (let ([x 42]) x)])
  (let ([x (let ([x (+ x 1)]) x)])
    (+ x x)))
