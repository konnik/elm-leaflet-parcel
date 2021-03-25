
import { Elm } from './src/Lab.elm'
import { registerApplication } from "./karta/ElmKarta"

var app = Elm.Lab.init({
    node: document.getElementById('elm')
});

registerApplication(app);

const sverigeExtent = [1118248.7982969885, 7696979.602230988, 2324119.356523609, 8764652.013318047]
const gavle = [60.72975, 17.12693];
const sverige = [63.031926, 15.451756]





