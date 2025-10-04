# purescript-elmish-time-machine

Time travel debug tool for purescript-elmish

## Usage

Just wrap the main `ComponentDef` with `withTimeMachine`:

```purs
main :: Effect Unit
main = defaultMain
  { def: withTimeMachine { init, update, view }
  , elementId: "app"
  }
```

#### Fast forward, rewind, pause, undo, and redo Elmish state transitions

![tardis1](https://github.com/user-attachments/assets/ebc26a8b-5982-4197-8c05-dbc1430cc21d)

#### Jump around to various states

![tardis3](https://github.com/user-attachments/assets/9b43082b-023a-4ed7-a7cd-188f9b3e10dd)

#### Inspect states and messages

![tardis2](https://github.com/user-attachments/assets/eb25be66-8791-4cf2-b487-cd187aeae2d2)
