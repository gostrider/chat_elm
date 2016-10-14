// var node = document.getElementById('app');
// var app = Elm.Component.B.embed(node);
// var app = Elm.Component.B.worker();

var app = Elm.Main.fullscreen();
app.ports.store.subscribe(function(valueToStore) {
    localStorage.setItem('key', valueToStore);
    console.log(valueToStore);
    app.ports.reply.send(JSON.stringify({
        "event" : "post",
        "response" : "ok"
    }));
});

app.ports.get.subscribe(function(key) {
    value = localStorage.getItem(key);
    app.ports.reply.send(JSON.stringify({
        "event" : "get",
        "response" : value 
    }));
});
