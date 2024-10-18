import { Elm } from "./Main.elm";

var app = Elm.Main.init({ node: document.getElementById("root") });

app.ports.scrollToProjectId.subscribe(function(idStr) {
    document.getElementById(idStr).scrollIntoView();
});