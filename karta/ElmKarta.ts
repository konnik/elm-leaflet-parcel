
import 'ol/ol.css';
import { skapa as skapaKarta, radera as raderaKarta, Karta } from "./Karta"

type Kartinstans = {
    karta?: Karta
    messages: any[]
}

const kartor: Map<string, Kartinstans> = new Map()

var sendToElm = (msg) => { }

export function registerApplication(app) {
    console.log("Registrerar Elm-app med portar:", app.ports);

    if (app.ports.kartaOutgoing) {
        app.ports.kartaOutgoing.subscribe(onMessage);
    } else {
        console.log("Varning: port 'kartaOutgoing' saknas. Inga händelser kommer att tas emot från Elm-appen.")
    }

    if (app.ports.kartaIncoming) {
        sendToElm = app.ports.kartaIncoming.send;
    } else {
        console.log("Varning: port 'kartaIncoming' saknas. Inga händelser kommer att skickas till Elm-appen.")
    }
}


function onMessage(message) {
    if (message.typ === "ny_karta") {
        // NY KARTA
        kartor.set(message.id, {
            karta: undefined,
            messages: []
        });

        sendToElm({
            "typ": "karta_skapad",
            "id": message.id
        });

        initMap(message.id)
    } else {
        const k = kartor.get(message.id)
        console.log("Lägger medelande i kö: ", message)
        k.messages.push(message)
        processMessages(k)
    }
}

function processMessages(k: Kartinstans) {
    if (!k.karta) {
        console.log("Karta inte initierad ännu, inga meddelanden behandades!")
        return
    }

    while (k.messages.length > 0) {
        processMessage(k.messages.shift())
    }
}

function processMessage(message) {
    console.log(message)

    const k = kartor.get(message.id)

    if (message.typ === "radera_karta") {
        raderaKarta(k.karta);
        kartor.delete(message.id);
    } else if (message.typ === "visa_lager") {
        k.karta.valjBakgrundslager(message.lagernamn);
    } else if (message.typ === "placera_kartnal") {
        k.karta.placeraMarkering(message.namn, [message.long, message.lat]);
    } else if (message.typ === "navigera_till") {
        k.karta.centreraKarta([message.long, message.lat]);
    } else {
        throw Error("okänd meddelandetyp: " + message.typ);
    }

}


function initMap(id: string) {
    var elem = document.getElementById(id);

    if (!elem) {
        console.log("Ingen kart-div '" + id + "' hittades, försöker igen senare.")
        setTimeout(() => initMap(id), 100)
        return;
    }
    console.log("Kart-div: ", elem)

    const k = kartor.get(id);

    k.karta = skapaKarta(id);
    k.karta.onEnkelklick(enkelklickHandler);
    kartor[id] = k;

    processMessages(k)
}

//     if (message.markering) {
//    kartinstans.placeraMarkering([message.markering.long, message.markering.lat]);
//    kartinstans.centreraKarta([message.markering.long, message.markering.lat]);
//}



function enkelklickHandler(karta, longLat) {
    sendToElm({
        "typ": "klick_i_karta",
        "id": karta.id,
        "lat": longLat[1],
        "long": longLat[0]
    });
};
