module App.Seq where
import App.Routes (Route(Home, NotFound))
import Pux.Html (Html, div, p, text)
import Pux.Html.Attributes (className)
import Prelude (id, const, ($), show, (<>))

type Year = Int 
type State = {
       name     :: String
     , acc      :: String
     , year     :: Year 
     , country  :: String
       }
data Action = PageView Route

view :: State -> Html Action
view state = div []
            [ p [className "name"]    [ text $ "Name:  " <> state.name ]
            , p [className "acc"]     [ text $ "Accession:  " <> state.acc ]
            , p [className "year"]    [ text $ "Year:  " <> show state.year ]
            , p [className "country"] [ text $ "Country:  " <> state.country ] ]

update :: Action -> State -> State
update = const id


init :: State -> State
init = id
