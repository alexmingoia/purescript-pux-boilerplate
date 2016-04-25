module App.Form where 
import App.Routes (Route)
import Prelude (($), map, (<>), show, const, (<<<), (&&), (<=), (>=), (<$>), (==), Eq, filter)
import Pux.Html (Html, text, form, button, input, span, ul)
import Pux.Html.Attributes (type_, value, name)
import Pux.Html.Events (FormEvent, onChange, onSubmit)
import Unsafe.Coerce (unsafeCoerce)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import App.Seq as Seq

type Year = Int

type State = {    name      :: Maybe String
                 , acc      :: Maybe String
                 , minYear  :: Year 
                 , maxYear  :: Year
                 , country  :: Maybe String
                 , result   :: Array Seq.State
              }
data Action =
   NameChange     FormEvent
 | MinYearChange  FormEvent
 | MaxYearChange  FormEvent
 | CountryChange  FormEvent
 | RunQuery
 | PageView Route
 | Child Seq.Action
  
init :: State
init = { name: Nothing, country: Nothing
       , minYear : 0, maxYear : 3000
       , acc : Nothing, result : [] }

update :: Action -> State -> State
update (RunQuery) state =   state { result = query state }
--update (RunState) state =   state { result = ((show state.minYear) <> (show state.maxYear)) } 
update (NameChange ev)    state = state { name =    Just ev.target.value }
update (CountryChange ev) state = state { country = Just ev.target.value }
update (MinYearChange ev) state = state { minYear = (unsafeCoerce ev.target.value) :: Int }
update (MaxYearChange ev) state = state { maxYear = (unsafeCoerce ev.target.value) :: Int }

view :: State -> Html Action
view state =
  form
  [ name "Search"
  , onSubmit (const RunQuery)
    ]
  [ input [ type_ "text", value $ fromMaybe "" state.name,    onChange NameChange ] []
  , input [ type_ "text", value $ fromMaybe "" state.country, onChange CountryChange ] []
  , input [ type_ "text", value $ show state.minYear, onChange MinYearChange ] []
  , input [ type_ "text", value $ show state.maxYear, onChange MaxYearChange ] []
  , button [ type_ "submit" ] [ text "Search" ]
  , ul [] $  map ((map Child) <<< Seq.view) state.result
    ]

seqs  = []
query :: State -> Array Seq.State
query q = filter match seqs
  where
    match x = (q.acc ==? x.acc) && 
      (q.name ==? x.name) &&
      (x.year >= q.minYear && x.year <= q.maxYear)
      --(q.serotype ==? x.serotype) &&
    (==?) :: forall a. (Eq a) => Maybe a -> a -> Boolean
    (==?) a b = fromMaybe true ((== b) <$> a)
