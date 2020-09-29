
import { Elm } from './src/Main.elm'
import karta from './karta'

Elm.Main.init({
  node: document.getElementById('elm')
})

karta();



