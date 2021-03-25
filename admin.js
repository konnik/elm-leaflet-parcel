
import { Elm } from './src/Admin.elm'
import { registerApplication } from "./karta/ElmKarta"

var app = Elm.Admin.init({
  node: document.getElementById('elm')
});

registerApplication(app);
