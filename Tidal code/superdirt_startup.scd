// Basado en el ejemplo de configuración para SuperDirt
// https://raw.githubusercontent.com/musikinformatik/SuperDirt/develop/superdirt_startup.scd


/*
Opcional: cargarlo invocando
"path/to/my/superdirt_startup.scd".load

La SuperCollider startup fileestá en
Platform.userAppSupportDir +/+ "startup.scd",
o vía el menu File > Open startup file
*/


(
s.reboot { // las opciones del servidor sólo se actualizar al reiniciar el servidor
	// configurar el scsynth: aquí se pueden añadir opciones específicas en función del hardware
	// ver http://doc.sccode.org/Classes/ServerOptions.html
	s.options.numBuffers = 1024 * 256; // incrementar si se necesitan cargar más samples (default 1024 * 256)
	s.options.memSize = 8192 * 32; // incrementar en caso de mensajes "alloc failed" (default 8192 * 32)
	s.options.numWireBufs = 64; // incrementar en caso de mensajes "exceeded number of interconnect buffers" (default 64)
	s.options.maxNodes = 1024 * 32; // incrementar en caso de drop outs y el mensaje "too many nodes" (default 1024 * 32)
	s.options.numOutputBusChannels = 2; // número de canales de salida (default 2)
	// s.options.numOutputBusChannels = 12;
	s.options.numInputBusChannels = 2; // definir a los canales de entrada requeridos de ser necesario (default 2)
	// boot the server and start SuperDirt
	s.waitForBoot {
		~dirt.stop; // detener cualquier instancia anterior para evitar duplicados (si no hay esto no hace nada)
		~dirt = SuperDirt(2, s); // número de canales por órbita (default 2), incrementar si se quiere panear a través de más canales
		~dirt.loadSoundFiles; // cargar dirt samples, se puede pasar un directorio con comodín para cargar más samples
		// por ejemplo: ~dirt.loadSoundFiles("/Users/myUserName/Dirt/samples/*");
		~dirt.loadSoundFiles("/home/xavigo/Audio/tidalclub/samples-extra/*");
		~dirt.loadSoundFiles("/home/xavigo/Audio/Samples/Incertidumbre/*");
		s.sync; // opcionalmente: esperar a que se lean los samples
		 ~dirt.start(57120, 0 ! 12);   // escuchar puerto 57120, crear 12 buses mandando audio al canal 0
		// ~dirt.start(57120, [0, 2, 4, 6, 8, 10]); // asignación de salidas estéreo para distintas órbitas

		// opcional, para acceso conveniente a las órbitas desde sclang:
		(
			~d1 = ~dirt.orbits[0]; ~d2 = ~dirt.orbits[1]; ~d3 = ~dirt.orbits[2];
			~d4 = ~dirt.orbits[3]; ~d5 = ~dirt.orbits[4]; ~d6 = ~dirt.orbits[5];
			~d7 = ~dirt.orbits[6]; ~d8 = ~dirt.orbits[7]; ~d9 = ~dirt.orbits[8];
			~d10 = ~dirt.orbits[9]; ~d11 = ~dirt.orbits[10]; ~d12 = ~dirt.orbits[11];
		);

		// abajo en esta zona se puede añadir más código que se quiera llamar al inicio
		// esto asegura que el servidor y ~dirt están corriendo
		//
		// este código se puede mantener por separado para poder cambiar de set
		// usando "ruta/a/mi/archivo.scd".load y comentar distintas declaraciones de load
		// ...

		// StageMaster.activate;


	};

	s.latency = 0.3; // incrementar si hay muchos mensaje "late"


};
);