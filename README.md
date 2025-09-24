# purescript-elmish-time-machine

Time travel debug tool for purescript-elmish

<img width="2972" height="2038" alt="image" src="https://github.com/user-attachments/assets/3605e47c-5f29-436e-8acf-4e29e3741d24" />

## Usage

```purs
main :: Effect Unit
main = defaultMain
  { def: withTimeMachine { init, update, view }
  , elementId: "app"
  }
```
