module App.Layout where
import App.Form as Form
import App.Routes (Route(Home, NotFound))
import Prelude (($), map, (<>), show, const)
import Pux.Html (Html, div, h1, p, text, form, button, input, span)
import Pux.Html.Attributes (type_, value, name)
import Pux.Html.Events (FormEvent, onChange, onSubmit)
import Data.Foreign (readInt)
import Unsafe.Coerce
import Data.Either
import Data.Maybe


data Action
  = Child (Form.Action)
  | PageView Route

type State =
  { route :: Route
  , count :: Form.State }

init :: State
init =
  { route: NotFound
  , count: Form.init }

update :: Action -> State -> State
update (PageView route) state = state { route = route }
update (Child action) state = state { count = Form.update action state.count }

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text "Search for sequences" ]
    , p [] [ text "Change src/Layout.purs and watch me hot-reload." ]
    , case state.route of
        Home -> map Child $ Form.view state.count
        NotFound -> App.NotFound.view state
    ]
