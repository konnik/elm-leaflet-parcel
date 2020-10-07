
import { Elm } from './src/Main.elm'
import {init} from './karta'

var app = Elm.Main.init({
  node: document.getElementById('elm')
});

const karta = init([testeboan, gavlean, forsby, vavaren ]);

app.ports.karta.subscribe(function(message) {
    if (message.type === "invalidera") {
        setTimeout(function() { karta.invalidateSize() },1);
    }
});

import testeboan from "./forsar/brannsagen";
import gavlean from "./forsar/gavlean";
import forsby from "./forsar/forsby";
import vavaren from "./forsar/vavaren";




