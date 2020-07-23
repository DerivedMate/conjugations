import Ling
import Helpers
type Test = (String, Bool)

tConjugationAssembly :: Bool
tConjugationAssembly = and $ map aux sets
  where 
    sets = [
      [ "ser", "siendo", "sido"
      , "soy", "eres", "es"
      , "somos", "sois", "son"],
      [ "fallecer", "falleiendo", "fallecido"
      , "fallezco", "falleces", "fallece"
      , "fallecemos", "fallecéis", "fallecen"],
      [ "haber", "habiendo", "habido"
      , "he", "has", "hay"
      , "hemos", "habéis", "han"]
      ]
    aux set = 
      set == (map assemblePattern $ stemWIrreg set)

main :: IO ()
main = print tests
  where 
    tests = [
      ("conjugation de-/restructuring", tConjugationAssembly)] 
