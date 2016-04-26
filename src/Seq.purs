module App.Seq where
import App.Routes (Route(Home, NotFound))
import Pux.Html (Html, div, p, text, table, tr, td)
import Pux.Html.Attributes (className)
import Prelude (id, const, ($), show, (<>), (<$>), Eq, Show, (==), (&&))
import Data.Generic
import Data.Maybe (Maybe, fromMaybe)

data Host = Human | Mosquito
derive instance genericHost :: Generic Host
instance showHost :: Show Host where
    show = gShow 
instance eqHost :: Eq Host where
    eq = gEq
    
data Serotype = DENV1 | DENV2 | DENV3 | DENV4
derive instance genericSerotype :: Generic Serotype 
instance showSerotype :: Show Serotype where
    show = gShow 
instance eqSerotype :: Eq Serotype where
    eq = gEq

data Segment = PB1 | PB2 -- ... etc. 
instance showSegment :: Show Segment where
  show PB1 = "PB1"
  show PB2 = "PB2"
instance eqSegment :: Eq Segment where
  eq x y = (show x) == (show y) 

type Year = Int 
type State = {
       name     :: String
     , acc      :: String
     , year     :: Year 
     , country  :: String 
     , host     :: Host
     , serotype :: Serotype
     , sequence :: String
     , segment  :: Maybe Segment
       }
stateEq :: State -> State -> Boolean
stateEq x y = x.name == y.name &&
              x.acc  == y.acc  && 
              x.year  == y.year  && 
              x.country  == y.country  && 
              x.host  == y.host  && 
              x.serotype  == y.serotype  && 
              x.segment  == y.segment
              
data Action = PageView Route
--TODO: add action to delete/check (check for deletion / download)
view :: State -> Html Action
view state = table []
            [tr [] [ td [className "name"]    [ text $ "Name:  " <> state.name ]
            , td [className "acc"]     [ text $ "Accession:  " <> state.acc ]
            , td [className "year"]    [ text $ "Year:  " <> show state.year ]
            , td [className "country"] [ text $ "Country:  " <> state.country ] ]
          , tr []   [ td []  [ text $ "Host:  " <> show state.host ] 
                 ,  td  [] [ text $ "Serotype:  " <> show state.serotype ] 
                 ,  td  [] [ text $ "Segment:  "  <> (fromMaybe "n/a" $ show <$> state.segment)] ]]

update :: Action -> State -> State
update = const id

init :: State -> State
init = id
