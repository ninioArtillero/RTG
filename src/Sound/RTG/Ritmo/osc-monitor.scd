
// La siguiente función permite monitorear los mensajes OSC recibidos en tiempo real.
OSCFunc.trace(true);


// Se apaga el servidor para evitar el ruido de los avisos periódicos de scsynth
// Estos son mensajes con la dirección por defecto del servidor (127.0.0.1, 57110)
// El interprete de SuperCollider (sclang) escucha por defecto en el puerto 57120
s.quit;

// Ahora al arrancar Tidal (desde el editor de texto o terminal),
// se detecta un "handshake" buscando a SuperDirt que se repite periódicamente.

// Corremos en Tidal `once $ sound "clap"` y detenemos el monitor de OSC
OSCFunc.trace(false);

/*
Se encuentra un mensaje similar a:

OSC Message Received:
	time: 1638.8551380129
	address: a NetAddr(127.0.0.1, 6010)
	recvPort: 57120
	msg: [ /dirt/play, cps, 0.5625, cycle, 0.0, delta, 1.7777760028839, s, clap ]
*/
