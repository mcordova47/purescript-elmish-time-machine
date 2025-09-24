# purescript-elmish-time-machine

Time travel debug tool for purescript-elmish

![492104689-edd68077-110e-4d9c-8433-a4207b505644](https://github.com/user-attachments/assets/d399f0ea-8238-4d86-84b2-f6cbe4c7d480)

## Usage

```purs
main :: Effect Unit
main = defaultMain
  { def: withTimeMachine { init, update, view }
  , elementId: "app"
  }
```
