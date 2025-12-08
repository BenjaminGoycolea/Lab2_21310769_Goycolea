:- consult(main_21310769_GoycoleaContardo).

:- 
set_prolog_flag(answer_write_options,[max_depth(0)]),
    
    write("===== PRUEBA COMPLETA - TODOS LOS RF (01-26) =====\n\n"),
    
    % RF01, RF03: TDA LIBRO
    write("=== RF01, RF03: TDA LIBRO ===\n"),
    crearLibro(101, "El Hobbit", "J.R.R. Tolkien", Libro1),
    crearLibro(102, "1984", "George Orwell", Libro2),
    crearLibro(103, "Cien Anos de Soledad", "Gabriel Garcia Marquez", Libro3),
    
    write("RF01 - Crear libro: "), getLibroTitulo(Libro1, Titulo1), write(Titulo1),
    write(" por "), getLibroAutor(Libro1, Autor1), write(Autor1), nl,
    write("RF03 - Selector ID: "), getLibroId(Libro1, Id1), write(Id1), nl,
    write("RF03 - Selector disponible: "), (isLibroDisponible(Libro1) -> write("true") ; write("false")), nl,
    write("RF03 - Modificar disponibilidad: "),
    setLibroDisponible(Libro1, false, Libro1Ocupado),
    (isLibroDisponible(Libro1Ocupado) -> write("true") ; write("false")),
    write(" (ahora no disponible)\n\n"),
    
    % RF02, RF13: TDA USUARIO
    write("=== RF02, RF13: TDA USUARIO ===\n"),
    crearUsuario(1, "Ana Garcia", Usuario1),
    crearUsuario(2, "Carlos Lopez", Usuario2),
    
    setUsuarioDeuda(Usuario2, 150, Usuario2ConDeuda),
    setUsuarioSuspendido(Usuario2ConDeuda, true, Usuario2Suspendido),
    agregarLibroUsuario(Usuario2Suspendido, 101, Usuario2ConLibros1),
    agregarLibroUsuario(Usuario2ConLibros1, 102, Usuario2ConLibros),
    
    write("RF02 - Crear usuario: "), getUsuarioNombre(Usuario1, NombreU1), write(NombreU1), nl,
    write("RF02 - Selector ID: "), getUsuarioId(Usuario1, IdU1), write(IdU1), nl,
    write("RF02 - Selector deuda: "), getUsuarioDeuda(Usuario2ConLibros, DeudaU2), write(DeudaU2), nl,
    write("RF02 - Selector libros: "), getUsuarioLibros(Usuario2ConLibros, LibrosU2), write(LibrosU2), nl,
    write("RF13 - Modificar deuda: "), setUsuarioDeuda(Usuario1, 200, Usuario1ConDeuda),
    getUsuarioDeuda(Usuario1ConDeuda, DeudaU1), write(DeudaU1), nl,
    write("RF13 - Modificar suspension: "), setUsuarioSuspendido(Usuario1, true, Usuario1Suspendido),
    (isUsuarioSuspendido(Usuario1Suspendido) -> write("true") ; write("false")), nl,
    write("RF13 - Verificar suspension: "),
    (isUsuarioSuspendido(Usuario2ConLibros) -> write("true") ; write("false")), nl,
    write("RF14 - Obtener deuda: "), obtenerDeuda(Usuario2ConLibros, DeudaObt), write(DeudaObt), nl,
    nl,
    
    % RF04, RF15, RF16, RF17: TDA PRESTAMO
    write("=== RF04, RF15, RF16, RF17: TDA PRESTAMO ===\n"),
    fecha(2025, 1, 3, Fecha0103),
    crearPrestamo(1, 1, 101, Fecha0103, 7, Prestamo1),
    
    write("RF04 - Crear prestamo: ID "), getPrestamoId(Prestamo1, IdP1), write(IdP1),
    write(", usuario "), getPrestamoIdUsuario(Prestamo1, IdUsuP1), write(IdUsuP1),
    write(", libro "), getPrestamoIdLibro(Prestamo1, IdLibP1), write(IdLibP1), nl,
    write("RF15 - Fecha prestamo: "), getPrestamoFechaPrestamo(Prestamo1, FechaP1), write(FechaP1), nl,
    write("RF15 - Dias solicitados: "), getPrestamoDiasSolicitados(Prestamo1, DiasP1), write(DiasP1), nl,
    write("RF15 - Fecha vencimiento: "), obtenerFechaVencimiento(Prestamo1, FechaVenc1), write(FechaVenc1), nl,
    
    fecha(2025, 10, 3, Fecha1003),
    write("RF16 - Dias retraso (vence 08/03, hoy 10/03): "),
    calcularDiasRetraso(FechaVenc1, Fecha1003, DiasRetraso1), write(DiasRetraso1), nl,
    
    write("RF17 - Calcular multa (2 dias * $100): $"),
    calcularMulta(Prestamo1, Fecha1003, 100, Multa1), write(Multa1), nl,
    nl,
    
    % RF05: CREAR BIBLIOTECA
    write("=== RF05: CREAR BIBLIOTECA ===\n"),
    fecha(2025, 1, 3, FechaInicial),
    crearBiblioteca([], [], [], 3, 14, 100, 500, 7, FechaInicial, BibliotecaInicial),
    write("Biblioteca creada con fecha inicial: "), getFecha(BibliotecaInicial, FechaIni), write(FechaIni), nl,
    write("Configuracion: max-libros=3, max-dias=14, multa=$100/dia, limite-deuda=$500, max-retraso=7\n\n"),
    
    % RF06: AGREGAR LIBRO
    write("=== RF06: AGREGAR LIBRO ===\n"),
    agregarLibro(BibliotecaInicial, Libro1, Bib1),
    agregarLibro(Bib1, Libro2, Bib2),
    agregarLibro(Bib2, Libro3, Bib3),
    write("Agregados 3 libros a la biblioteca\n"),
    getBibliotecaLibros(Bib3, LibrosBib3),
    length(LibrosBib3, NumLibros3),
    write("Total libros: "), write(NumLibros3), nl,
    
    crearLibro(101, "Otro Titulo", "Otro Autor", LibroDuplicado),
    agregarLibro(Bib3, LibroDuplicado, Bib3Intento),
    write("Intento agregar libro con ID duplicado: "),
    (Bib3 == Bib3Intento -> write("RECHAZADO") ; write("ACEPTADO")), nl,
    nl,
    
    % RF07: REGISTRAR USUARIO
    write("=== RF07: REGISTRAR USUARIO ===\n"),
    registrarUsuario(Bib3, Usuario1, Bib4),
    registrarUsuario(Bib4, Usuario2ConLibros, Bib5),
    write("Registrados 2 usuarios en el sistema\n"),
    getBibliotecaUsuarios(Bib5, UsuariosBib5),
    length(UsuariosBib5, NumUsuarios5),
    write("Total usuarios: "), write(NumUsuarios5), nl,
    
    crearUsuario(1, "Otro Nombre", UsuarioDuplicado),
    registrarUsuario(Bib5, UsuarioDuplicado, Bib5Intento),
    write("Intento registrar usuario con ID duplicado: "),
    (Bib5 == Bib5Intento -> write("RECHAZADO") ; write("ACEPTADO")), nl,
    nl,
    
    % RF08: OBTENER USUARIO
    write("=== RF08: OBTENER USUARIO ===\n"),
    obtenerUsuario(Bib5, 1, UsuarioEncontrado),
    write("Usuario ID 1: "), getUsuarioNombre(UsuarioEncontrado, NombreEnc), write(NombreEnc), nl,
    obtenerUsuario(Bib5, 999, UsuarioInexistente),
    write("Usuario ID 999: "),
    (UsuarioInexistente = false -> write("NO ENCONTRADO") ; write("ENCONTRADO")), nl,
    nl,
    
    % RF09: BUSCAR LIBRO
    write("=== RF09: BUSCAR LIBRO ===\n"),
    buscarLibro(Bib5, "id", 101, LibroPorId),
    write("Buscar por ID 101: "), getLibroTitulo(LibroPorId, TituloBuscado), write(TituloBuscado), nl,
    buscarLibro(Bib5, "titulo", "hobbit", LibroPorTitulo),
    write("Buscar por titulo 'hobbit': "), getLibroTitulo(LibroPorTitulo, TituloBusc2), write(TituloBusc2), nl,
    buscarLibro(Bib5, "autor", "tolkien", LibroPorAutor),
    write("Buscar por autor 'tolkien': "), getLibroAutor(LibroPorAutor, AutorBusc), write(AutorBusc), nl,
    buscarLibro(Bib5, "titulo", "inexistente", LibroNoExiste),
    write("Buscar libro inexistente: "),
    (LibroNoExiste = false -> write("NO ENCONTRADO") ; write("ENCONTRADO")), nl,
    nl,
    
    % RF10, RF11: GET-LIBRO-ID, GET-FECHA
    write("=== RF10, RF11: GET-LIBRO-ID, GET-FECHA ===\n"),
    write("RF10 - ID del libro: "), getLibroId(Libro1, IdLibro10), write(IdLibro10), nl,
    write("RF11 - Fecha actual sistema: "), getFecha(Bib5, FechaActual11), write(FechaActual11), nl,
    nl,
    
    % RF12: LIBRO DISPONIBLE
    write("=== RF12: LIBRO DISPONIBLE ===\n"),
    write("Libro 101 disponible: "),
    (isLibroDisponible(Bib5, 101) -> write("true") ; write("false")), nl,
    write("Libro 999 disponible: "),
    (isLibroDisponible(Bib5, 999) -> write("true") ; write("false")),
    write(" (libro no existe)\n"),
    nl,
    
    % RF18: TOMAR PRESTAMO
    write("=== RF18: TOMAR PRESTAMO ===\n"),
    crearUsuario(3, "Pedro Activo", UsuarioActivo),
    registrarUsuario(Bib5, UsuarioActivo, Bib6),
    
    write("Usuario 3 toma libro 101 por 7 dias\n"),
    tomarPrestamo(Bib6, 3, 101, 7, FechaInicial, Bib7),
    write("Prestamo realizado: "),
    (Bib6 == Bib7 -> write("NO") ; write("SI")), nl,
    getBibliotecaPrestamos(Bib7, PrestamosBib7),
    length(PrestamosBib7, NumPrestamos7),
    write("Total prestamos: "), write(NumPrestamos7), nl,
    write("Libro 101 ahora disponible: "),
    (isLibroDisponible(Bib7, 101) -> write("true") ; write("false")), nl,
    
    write("Usuario 1 intenta tomar mismo libro: "),
    (\+ tomarPrestamo(Bib7, 1, 101, 5, FechaInicial, _) -> write("RECHAZADO (libro ocupado)") ; write("ACEPTADO")), nl,
    
    write("Usuario 2 suspendido intenta tomar libro 102: "),
    (\+ tomarPrestamo(Bib7, 2, 102, 5, FechaInicial, _) -> write("RECHAZADO (suspendido)") ; write("ACEPTADO")), nl,
    nl,
    
    % RF19: DEVOLVER LIBRO
    write("=== RF19: DEVOLVER LIBRO ===\n"),
    procesarDia(Bib7, BibD1), procesarDia(BibD1, BibD2), procesarDia(BibD2, BibD3),
    procesarDia(BibD3, BibD4), procesarDia(BibD4, BibD5), procesarDia(BibD5, BibD6),
    procesarDia(BibD6, BibD7), procesarDia(BibD7, BibD8), procesarDia(BibD8, Bib8),
    write("Fecha avanzada a: "), getFecha(Bib8, FechaAvanz), write(FechaAvanz), nl,
    
    obtenerUsuario(Bib8, 3, Usuario3Antes),
    write("Deuda usuario 3 antes devolucion: $"), getUsuarioDeuda(Usuario3Antes, DeudaAntes), write(DeudaAntes), nl,
    
    write("Usuario 3 devuelve libro 101 con retraso\n"),
    getFecha(Bib8, FechaActualDev),
    devolverLibro(Bib8, 3, 101, FechaActualDev, Bib9),
    
    obtenerUsuario(Bib9, 3, Usuario3Despues),
    write("Deuda usuario 3 despues devolucion: $"), getUsuarioDeuda(Usuario3Despues, DeudaDespues), write(DeudaDespues), nl,
    write("Libro 101 disponible nuevamente: "),
    (isLibroDisponible(Bib9, 101) -> write("true") ; write("false")), nl,
    nl,
    
    % RF20: DEBE SUSPENDERSE
    write("=== RF20: DEBE SUSPENDERSE ===\n"),
    getFecha(Bib9, FechaActual20),
    write("Usuario 3 debe suspenderse: "),
    (debeSuspenderse(Bib9, 3, FechaActual20) -> write("true") ; write("false")), nl,
    write("Usuario 1 debe suspenderse: "),
    (debeSuspenderse(Bib9, 1, FechaActual20) -> write("true") ; write("false")), nl,
    nl,
    
    % RF21: SUSPENDER USUARIO
    write("=== RF21: SUSPENDER USUARIO ===\n"),
    obtenerUsuario(Bib9, 1, Usuario1AntesSuspension),
    write("Usuario 1 suspendido antes: "),
    (isUsuarioSuspendido(Usuario1AntesSuspension) -> write("true") ; write("false")), nl,
    
    suspenderUsuario(Bib9, 1, Bib10),
    obtenerUsuario(Bib10, 1, Usuario1DespuesSuspension),
    write("Usuario 1 suspendido despues: "),
    (isUsuarioSuspendido(Usuario1DespuesSuspension) -> write("true") ; write("false")), nl,
    nl,
    
    % RF22: RENOVAR PRESTAMO
    write("=== RF22: RENOVAR PRESTAMO ===\n"),
    crearUsuario(4, "Maria Renovadora", Usuario4),
    registrarUsuario(Bib10, Usuario4, Bib11),
    getFecha(Bib11, FechaActual22),
    tomarPrestamo(Bib11, 4, 102, 5, FechaActual22, Bib12),
    
    write("Usuario 4 toma libro 102 por 5 dias\n"),
    getBibliotecaPrestamos(Bib12, PrestamosBib12),
    member(PrestamoOriginal, PrestamosBib12),
    getPrestamoIdUsuario(PrestamoOriginal, 4),
    getPrestamoId(PrestamoOriginal, IdPrestamoRenovar),
    write("Dias originales: "), getPrestamoDiasSolicitados(PrestamoOriginal, DiasOriginales), write(DiasOriginales), nl,
    
    write("Renovando prestamo por 3 dias extra\n"),
    getFecha(Bib12, FechaActual22b),
    renovarPrestamo(Bib12, IdPrestamoRenovar, 3, FechaActual22b, Bib13),
    
    getBibliotecaPrestamos(Bib13, PrestamosBib13),
    member(PrestamoRenovado, PrestamosBib13),
    getPrestamoIdUsuario(PrestamoRenovado, 4),
    write("Dias renovados: "), getPrestamoDiasSolicitados(PrestamoRenovado, DiasRenovados), write(DiasRenovados), nl,
    nl,
    
    % RF23: PAGAR DEUDA
    write("=== RF23: PAGAR DEUDA ===\n"),
    obtenerUsuario(Bib13, 3, Usuario3ConDeuda),
    write("Deuda usuario 3: $"), getUsuarioDeuda(Usuario3ConDeuda, DeudaU3), write(DeudaU3), nl,
    write("Usuario 3 suspendido: "),
    (isUsuarioSuspendido(Usuario3ConDeuda) -> write("true") ; write("false")), nl,
    
    write("Usuario 3 paga $100\n"),
    pagarDeuda(Bib13, 3, 100, Bib14),
    obtenerUsuario(Bib14, 3, Usuario3PagoParcial),
    write("Deuda restante: $"), getUsuarioDeuda(Usuario3PagoParcial, DeudaRestante), write(DeudaRestante), nl,
    write("Sigue suspendido: "),
    (isUsuarioSuspendido(Usuario3PagoParcial) -> write("true") ; write("false")), nl,
    
    write("Usuario 3 paga deuda restante\n"),
    getUsuarioDeuda(Usuario3PagoParcial, DeudaAPagar),
    pagarDeuda(Bib14, 3, DeudaAPagar, Bib15),
    obtenerUsuario(Bib15, 3, Usuario3Liberado),
    write("Deuda final: $"), getUsuarioDeuda(Usuario3Liberado, DeudaFinal), write(DeudaFinal), nl,
    write("Suspendido final: "),
    (isUsuarioSuspendido(Usuario3Liberado) -> write("true") ; write("false")), nl,
    nl,
    
    % RF24: HISTORIAL PRESTAMOS USUARIO
    write("=== RF24: HISTORIAL PRESTAMOS USUARIO ===\n"),
    historialPrestamosUsuario(Bib15, 3, HistorialUsuario3),
    write("Historial usuario 3:\n"),
    write(HistorialUsuario3),
    nl,
    
    % RF25: HISTORIAL PRESTAMOS SISTEMA
    write("=== RF25: HISTORIAL PRESTAMOS SISTEMA ===\n"),
    historialPrestamosSistema(Bib15, HistorialCompleto),
    write("Historial completo del sistema:\n"),
    write(HistorialCompleto),
    nl,
    
    % RF26: PROCESAR DIA
    write("=== RF26: PROCESAR DIA ===\n"),
    write("Fecha antes: "), getFecha(Bib15, FechaAntes26), write(FechaAntes26), nl,
    procesarDia(Bib15, Bib16),
    write("Fecha despues: "), getFecha(Bib16, FechaDespues26), write(FechaDespues26), nl,
    
    procesarDia(Bib16, Bib17), procesarDia(Bib17, Bib18), procesarDia(Bib18, Bib19),
    procesarDia(Bib19, Bib20), procesarDia(Bib20, Bib21), procesarDia(Bib21, Bib22),
    procesarDia(Bib22, Bib23), procesarDia(Bib23, Bib24), procesarDia(Bib24, Bib25),
    procesarDia(Bib25, Bib26), procesarDia(Bib26, Bib27), procesarDia(Bib27, Bib28),
    procesarDia(Bib28, Bib29), procesarDia(Bib29, Bib30), procesarDia(Bib30, Bib31),
    procesarDia(Bib31, Bib32), procesarDia(Bib32, Bib33), procesarDia(Bib33, Bib34),
    procesarDia(Bib34, BibNuevoMes),
    
    write("Fecha despues de procesar hasta fin de mes: "), 
    getFecha(BibNuevoMes, FechaNuevoMes), write(FechaNuevoMes), nl,
    nl,
    
    write("\n===== FIN DE PRUEBAS - TODOS LOS RF PROBADOS =====\n").
