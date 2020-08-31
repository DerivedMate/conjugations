# Conjugations
## What is it?
The aim of this project is to algorithmically analyse the conjugation(s) (or any variable part of a language; such as declension etc.) of any language thrown at it. Currently it's set to process verb conjugations from [SpanishDict](https://www.spanishdict.com/), but it can be set to process any set of data by employing a custom `ConjugationExtraction` (`FilePath -> IO [[[String]]]`) to the `processVerb` function (@Main:48:25).

As of now, the data is grouped as follows:
```haskell
  Conjugation :: [[[String]]]
       ∧    
      Mood    ::  [[String]]
       ∧      
     Tense    ::   [String]
```

## The algorithm
A custom intersection algorithm had to be employed, because:
1. Haskell's built-in `intersect` does not provide a quantitative intersection, which means that:
    ```
    let A, B be sets
    let x ∈ A ∧ B

    even though
      count(x, A) = 2 and count(x, B) = 1, 
      count(x, A ∧ B) = 2
    ```
    A great example of this behavior can be found in the pair: *"fuere"* and *"fuéremos"*
2. The aforementioned intersection is not ordered, which makes it harder to detect transformations (i.e: `'ue' <-> 'o'`in the verb *poder*)
