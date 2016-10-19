// var node = document.getElementById('app');
// var app = Elm.Component.B.embed(node);
// var app = Elm.Component.B.worker();

function storageAvailable(type) {
    try {
		var storage = window[type],
            x = '__storage_test__';
		storage.setItem(x, x);
		storage.removeItem(x);
		return true;
	}
	catch(e) {
        return false;
	}
}

var app = Elm.Main.fullscreen();

app.ports.store.subscribe(function(valueToStore) {
    var result = "";
    if (storageAvailable('localStorage')) {
        localStorage.setItem('user_auth', valueToStore);
        result = "ok";
    } else {
        result = "error";
    }
    console.log(result + "_" + valueToStore);
    app.ports.reply.send(JSON.stringify({
        "event" : "post",
        "response" : result
    }));
});

app.ports.get.subscribe(function(key) {
    var value = localStorage.getItem(key);
    if (value === null) value = "error";
    try {
        value = JSON.parse(value);
        app.ports.reply.send(JSON.stringify({
            "event" : "get",
            "response" : value
        }));
    } catch (e) {
        app.ports.reply.send(JSON.stringify({
            "event" : "error",
            "response" : "failed_parseJSON"
        }));
    }

});
