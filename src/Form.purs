module App.Form where 
import App.Routes (Route(Home, NotFound))
import Prelude (($), map, (<>), show, const)
import Pux.Html (Html, div, h1, p, text, form, button, input, span)
import Pux.Html.Attributes (type_, value, name)
import Pux.Html.Events (FormEvent, onChange, onSubmit)
import Data.Foreign (readInt)
import Unsafe.Coerce
import Data.Either
import Data.Maybe
type Year = Int
type State = {    name      :: Maybe String
                 , acc      :: Maybe String
                 , minYear  :: Year

                 , maxYear  :: Year
                 , country  :: Maybe String
                 , result   :: String }
data Action =
    NameChange     FormEvent
 |  MinYearChange  FormEvent
 |  MaxYearChange  FormEvent
 |  CountryChange  FormEvent
 |  RunState
 |  PageView Route
  
init :: State
init = { name: Nothing, country: Nothing
       , minYear : 0, maxYear : 3000
       , acc : Nothing, result : "None yet" }

update :: Action -> State -> State
update (RunState) state =   state { result = ((show state.minYear) <> (show state.maxYear)) } 
--update (RunState) state =   state { result = ((show state.minYear) ++ (show state.maxYear)) } 
update (NameChange ev)    state = state { name =    Just ev.target.value }
update (CountryChange ev) state = state { country = Just ev.target.value }
update (MinYearChange ev) state = state { minYear = (unsafeCoerce ev.target.value) :: Int }
update (MaxYearChange ev) state = state { maxYear = (unsafeCoerce ev.target.value) :: Int }

view :: State -> Html Action
view state =
  form
  [ name "Search"
  , onSubmit (const RunState)
    ]
  [ input [ type_ "text", value $ fromMaybe "" state.name,    onChange NameChange ] []
  , input [ type_ "text", value $ fromMaybe "" state.country, onChange CountryChange ] []
  , input [ type_ "text", value $ show state.minYear, onChange MinYearChange ] []
  , input [ type_ "text", value $ show state.maxYear, onChange MaxYearChange ] []
  , button [ type_ "submit" ] [ text "Search" ]
  , span [] [ text (state.result)]
    ]
