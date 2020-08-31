processVerb :: Maybe PVT -> (Int, FilePath) -> IO (Maybe PVT)
processVerb f0 (i, path) = do
  cons       <- extractConjugations path
  categories <- pure 
                $ makeFormal
                $ categoryOfVerb
                $ map 
                  (map stemWIrreg) 
                  (normalizeConjugations cons)

  let 
    verb       = verbalize path
    mapF       = map (Group [verb])
    f1         = formalIdentify 
                 $ formalMap mapF mapF mapF categories
    in pure $ Just $ 
      case f0 of
        Just f0 -> mergeFormals f0 f1
        Nothing -> f1