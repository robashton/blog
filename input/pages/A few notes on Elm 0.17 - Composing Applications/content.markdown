Last little 'note' on Elm

- [A few notes - Intro](/entries/a-few-notes-on-elm-0.17---intro.html)
- [A few notes - The Language](/entries/a-few-notes-on-elm-0.17---the-language.html)
- A few notes - Composing Applications

One of the most common questions in the Slack channels right now is 

<pre>
  "How do I do parent/child communication in Elm 0.17"
</pre>

At the heart of it all is [The Elm Architecture](http://guide.elm-lang.org/architecture/index.html) enouraging developers to write modules that look like this

```haskell
    import Html exposing (..)

    type alias Model = { ... }
    type Msg = Reset | ...

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
      case msg of
        Reset -> handleReset model

    view : Model -> Html Msg
    view model = someHtml model
```


This seems inherently sensible and you can nest components simply by storing their child models on the parent model, and calling into the child 'view' and 'update' functions with messages and that model that are entirely opaque to the parent.

This is great, two things stand out here

- Unlike in React, control over the state you use is kept inside the component (updates/etc)
- How do you get useful events back up to the parent or send commands to the child when something changes?

The first one is a bit weird because it means while you have a top level state object most of it is generally opaque to the code at the top level. A component will create its initial state, send messages to itself to update its state and then use that state to render. The "parent" then has boilerplate to route those messages. Components are therefore entirely standalone.

We are currently rolling with the following concepts for our Elm apps.

- *Routing*: Top level component, doesn't have any state other than current page and current model. Handles top level shared events (more on this coming up)
- *Pages*: isolated components following the init/update/view/subscriptions pattern and doing all their own data loading/saving internally
- *Children*: Mostly don't require their own models, just dumb rendering functions, no init/update/etc. Raise events from attributes passed in in 'view'

There are some exceptions to Children, but mostly events come from the DOM and we can just wire them up pretty much directly without any need for intermediate models. You don't tend to need much more nesting than this. I'm a bit concerned that the Html module seems to load its source of truth from the dom instead of relying on the model passed in but *shrug*.

What this effectively means is that each Page holds the "Single Source of Truth" and simply renders a tree based on that source of truth, wires up events from that tree into messages within the page, handles those messages, updates the single source of truth and the cycle goes on. 

```haskell
    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model = 
        case msg of 
          MyClickMessage -> ( model, loadSomethingCool )

    view : Model -> Html.Msg
    view model ->
       MyButton.default [ onClick MyClickMessage ] "Click me!"
```

There are some things that need to be handled globally however, changes to navigation, displaying generic error messages/feedback, dialog management (okay, not dialog management, don't use dialogs please), global data management (which user are we?) and sometimes we need to 

- Tell a Page that something has changed while it wasn't looking
- Tell the Router that we want something to happen

There have been (in the last few days) a couple of posts written on this subject

- [Child/Parent Communication in Elm](http://folkertdev.nl/blog/elm-child-parent-communication)
- [The Translator Pattern](https://medium.com/@alex.lew/f4bfaa1d3f98#.otdxecxl8)

Our current preferred method for getting messages to the Router is detailed in the first post and looks a bit like this

*Update method in Page*
```haskell
  update : Msg -> Model -> ( Model, Cmd Msg, Cmd Common.Event )
  update msg model =
      case msg of
          FetchFail err ->
              handleError err model

          Tick ->
              ( model, Cmd.none, Cmd.none )

          ReloadSomeState ->
              ( model, updateSomeState, Cmd.none )

          VisitResource id ->
              changeRoute (ResourcePageRoute id) model

          FetchSomeStateSucceed state ->
              ( { model | SomeState = Just state }, Cmd.none, Cmd.none )

```

See the tuple? Pages return a triplet where the third item can be something from Common.Event. Common also exports ways of making those events to keep life easy for us in our Pages.

```haskell

    changeRoute : Route -> model -> ( model, Cmd msg, Cmd Event )
    changeRoute route model =
        ( model, Cmd.none, event (RouteChanged route) )

```

I wouldn't try and force this any further down than the Page level, most Components below this can get away with just using [ onEvent SomeMsg ] and we don't want too much child->parent communication anyway, it's usually indicative that we're trying to keep state in the wrong place.

Our current preferred method for getting messages from the Router to the Child (this is very rare) is simply to export a method from the child component which knows what sort of message to return.

```haskell
    module Child exposing (Msg, Model, doSomethingInteresting) 

    doSomethingInteresting : SomeArgumentType -> Model -> ( Model, Cmd Msg )
    doSomethingInteresting arg model = ( model, kickTaskOffFor arg )

```

This can just be called in the 'update' method of the parent.

The ugly bit of routing
==

Dynamic dispatch in a typed language is a pain and currently we have a lot of boilerplate around child components, it looks a bit like this in our 'update' function in our Router, you can easily imagine our init/subscriptions/view methods.


```haskell

    bubble : (a -> Msg) -> Cmd a -> Cmd Common.Event -> Cmd Msg
    bubble lifter cmd ev =
        Cmd.batch
            [ Cmd.map lifter cmd
            , Cmd.map Event ev
            ]

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            HomePageMsg act ->
                let
                    ( newPage, cmd, ev ) =
                        HomePage.update act model.homePage
                in
                    ( { model | homePage = newPage }, bubble HomePageMsg cmd ev )

            ResourcePageMsg act ->
                let
                    ( newPage, cmd, ev ) =
                        ResourcePage.update act model.resourcePage
                in
                    ( { model | resourcePage = newPage }, bubble ResourcePageMsg cmd ev )

            Event ev ->
                case ev of
                    Common.RouteChanged newRoute ->
                        ( model, newUrl (toHash newRoute) )

                    Common.HttpError err ->
                        Debug.log (toString err) ( model, Cmd.none )

```

That's a whopping case clause for each child component. It's avoidable if they all share the same model and you can do tricks by lifting the model into a parametric type but you still need to write a clause for each child because you can't have a heterogenous list of 'routes' to dispatch from.

I haven't seen a tidy way of doing this yet, an example of lifting the child models into a parametric type can be found in the [elm-parts](https://github.com/debois/elm-parts) repo, but it's a bit invasive while not adding all that much (it's a good start though).

I think if records had 'setter' functions then most of these functions for update/init/view/etc could be generated on start-up by calling a builder repeatedly with each route, something worth musing on but not seen in the wild yet.


Some modules
==

We're using [elm navigation](https://github.com/elm-lang/navigation) and [evancz UrlParser](https://github.com/evancz/url-parser) to do our routing and that's all fairly simple, just onerous as far as updates go. 

Nuff said, I don't have opinions beyond "this is going to get a bit clunky when our web-app has a few dozen routes. There is an argument that we should just do this on the server and go all progressive-enhancement on our app - no thanks, we're using Erlang for most of our apps and it's not a path we want to tread.
