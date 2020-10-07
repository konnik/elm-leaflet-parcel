module Fors exposing (beskrivning)

import Element exposing (Element)


beskrivning : Element msg
beskrivning =
    String.lines txt
        |> List.take 4
        |> List.map Element.text
        |> Element.paragraph [ Element.spacing 20 ]


txt : String
txt =
    """Testeboån.
    
Ån rinner genom ett flackt skogs- och myrlandskap med gles bebyggelse och få vägar. Testeboån var tidigare flottled och spår efter detta finns kvar på sina håll. Det är slutet på denna å mellan Brännsågen och Åbyggeby som är nåt att ha för forspaddlare. Uppströms är det fint kanotvatten. Testeboån har klassats som extra skyddsvärd av staten p.g.a. av dess unika natur och fauna. Här finns bl.a. flodpärlmusslor.

Aktuell vattenföring och prognos från SMHI (följ länken och klicka sedan på "konstdalsströmmen" för prognos.

Hitta hit: Testeboån ligger strax norr om Gävle. Det finns ingen avtagsväg från E4:an.så man måste köra genom Gävle eller västerifrån från väg 272. Kör genom Forsby och Åbyggeby, som är take out. Put in ca 7 km nordväst om Åbyggeby, vid Brännsågen. När du kör över ån så är du framme, det finns en bra parkering på höger sida. Take out nere vid bron i Åbyggeby, det går bra att ställa en bil vid bastun. Vill man förlänga turen så kör man ner till Forsbybadet ca 2 km nedströms. Sväng av vid ett brunt hus intill en busshållsplats. Stor parkering vid vindskyddet.

Det går numer att paddla ända ner till Forsbybadet, ca 2 km nedströms för där har man rivit kraftverket, Här finns det nu en hyfsad fors där det blir några valsar. Vid riktigt mkt vatten kan de vara användbara, det är dock ganska grunt och stenigt, dessutom ligger den bästa valsen strax ovanför en bropelare. Det går fint att parkera här och det finns ett fint vindskydd. Värt besväret att paddla ner hit.

Det krävs att det passerar ca 25 m3/s vid Konstdalsströmmarna (som ligger en bit uppströms) för att paddlingen ska vara trevlig. Över 30 m3/s är riktigt bra.

Testeboån erbjuder fin forspaddling utan att vara extrem. Det kan vara bra att reka den andra riktiga forsen, den som börjar med en kraftig högersväng. Här finns det ett par större stenar mitt i forsen som kan stöka till det lite. I övrigt finns det inga direkt svårigheter utefter turen.

Dessutom finns det en opaddlad fors nere i Strömsbro vid den gamla textilfabriken Vävaren. Vi har rekat men då var det för lite vatten. Denna fors innehåller en del tekniska svårigheter och mindre drop.


Testeboån - 2.2a
Lite av en creekingtur faktiskt. Inget hårresande men helt ok. Många små fina bakvatten att plocka på vägen ner med några korta slätvattenssträckor. Inga stora surfvågor eller valsar, men en fin vårtur.
Kolla med Gävleklubben om de vill hänga på så kanske de kan fixa bybastun.

Hitta hit: Se ovan.
"""
