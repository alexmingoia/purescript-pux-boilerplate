module App.Form where
import Global (encodeURIComponent)
import App.Routes (Route)
import Prelude (($), map, (<>), show, const, (<<<), (&&), (<=), (>=), (<$>), (==), Eq)
import Pux.Html (Html, text, form, button, input, span, ul, div, a)
import Pux.Html.Attributes (type_, value, name, download, href)
import Pux.Html.Events (FormEvent, onChange, onSubmit, onClick)
import Unsafe.Coerce (unsafeCoerce)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import App.Seq as Seq
import Data.Array (filter, nubBy)
import Data.Foldable (intercalate)
type Year = Int

type State = {    name      :: Maybe String
                 , acc      :: Maybe String
                 , minYear  :: Year 
                 , maxYear  :: Year
                 , country  :: Maybe String
                 , segment  :: Maybe Seq.Segment
                 , host     :: Maybe Seq.Host
                 , serotype :: Maybe Seq.Serotype
                 , result   :: Array Seq.State
              }
data Action =
   NameChange     FormEvent
 | MinYearChange  FormEvent
 | MaxYearChange  FormEvent
 | CountryChange  FormEvent
 | HostChange     FormEvent
 | SerotypeChange FormEvent
 | SegmentChange  FormEvent
 | RunQuery
 | ResetResults
 | PageView Route
 | Child Seq.Action

  
init :: State
init = { name: Nothing, country: Nothing
       , host: Nothing, serotype: Nothing
       , segment: Nothing
       , minYear : 0, maxYear : 3000
       , acc : Nothing, result : [] }
-- In order to give Seq.State an Eq instance, it must be wrapped in NewType
update :: Action -> State -> State
update (RunQuery) state = state { result = nubBy Seq.stateEq $ state.result <> (query state) }
--update (RunState) state =   state { result = ((show state.minYear) <> (show state.maxYear)) } 
update (NameChange ev)    state = state { name =    Just ev.target.value }
update (CountryChange ev) state = state { country = Just ev.target.value }
update (MinYearChange ev) state = state { minYear = (unsafeCoerce ev.target.value) :: Int }
update (MaxYearChange ev) state = state { maxYear = (unsafeCoerce ev.target.value) :: Int }
update (HostChange ev)    state = state { host = Just (unsafeCoerce ev.target.value :: Seq.Host) }
update (SerotypeChange ev)    state = state { serotype = Just (unsafeCoerce ev.target.value :: Seq.Serotype) }
update (SegmentChange ev)    state = state { segment = Just (unsafeCoerce ev.target.value :: Seq.Segment) }
update ResetResults     state = state { result = [] }

view :: State -> Html Action
view state =
  div []
  [form
  [ name "Search"
  , onSubmit (const RunQuery)
    ]
  [ input [ name "Name:", type_ "text", value $ fromMaybe "" state.name,    onChange NameChange ] []
  , input [ name "Country:", type_ "text", value $ fromMaybe "" state.country, onChange CountryChange ] []
  , input [ name "Host Species:", type_ "text", value $ fromMaybe "" $ show <$> state.host, onChange HostChange ] []
  , input [ name "Segmeent (optional):", type_ "text", value $ fromMaybe "" $ show <$> state.segment, onChange SegmentChange ] []
  , input [ name "Serotype:", type_ "text", value $ fromMaybe "" $ show <$> state.serotype, onChange SerotypeChange ] []
  , input [ name "Minimum Year", type_ "text", value $ show state.minYear, onChange MinYearChange ] []
  , input [ name "Maximum Year", type_ "text", value $ show state.maxYear, onChange MaxYearChange ] []
  , button [ type_ "submit" ] [ text "Search" ]
  , ul [] $  map ((map Child) <<< Seq.view) state.result
    ]
  , button [ onClick (const ResetResults)] [ text "Reset" ]
  , a [href ("data:text/plain;charset=utf-8," <> (encodeURIComponent $ toCSV state.result)) ] [text "Download"]]

toCSV :: Array Seq.State -> String
toCSV xs = header <> "\n" <> (intercalate "\n" $ map toRow xs)
  where
    header = "name,acc,year,segment"
    toRow x = intercalate "," [x.name, x.acc, (show x.year), (show x.segment)]
    

query :: State -> Array Seq.State
query q = filter match seqs
  where
    match x = (q.acc ==? x.acc) && 
      (q.name ==? x.name) &&
      (x.year >= q.minYear && x.year <= q.maxYear)
      --(q.serotype ==? x.serotype) &&
    (==?) :: forall a. (Eq a) => Maybe a -> a -> Boolean
    (==?) a b = fromMaybe true ((== b) <$> a)

seqs  = [example]

example :: Seq.State
example =  {
       name     : "NameFoo"
     , acc      : "AccFoo"
     , year     : 1989
     , country  : "USA"
     , host     : Seq.Mosquito
     , serotype : Seq.DENV1
     , sequence : "ACTG"
     , segment  : Nothing
       }
