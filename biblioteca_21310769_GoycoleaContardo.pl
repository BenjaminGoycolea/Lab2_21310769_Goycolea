% Exportar predicados
:- module(biblioteca_21310769_GoycoleaContardo, [
    crearBiblioteca/10,
    agregarLibro/3,
    registrarUsuario/3,
    obtenerUsuario/3,
    buscarLibro/4,
    getFecha/2,
    isLibroDisponible/2,
    obtenerDeuda/2,
    getBibliotecaTasaMulta/2,
    getBibliotecaLibros/2,
    getBibliotecaUsuarios/2,
    getBibliotecaPrestamos/2,
    getBibliotecaMaxLibros/2,
    getBibliotecaDiasMax/2,
    getBibliotecaLimiteDeuda/2,
    getBibliotecaDiasRetraso/2,
    tomarPrestamo/6,
    devolverLibro/5,
    debeSuspenderse/3,
    suspenderUsuario/3,
    renovarPrestamo/5,
    pagarDeuda/4,
    historialPrestamosUsuario/3,
    historialPrestamosSistema/2,
    procesarDia/2,
    fecha/4
]).

% Importar los otros archivos
:- use_module(usuario_21310769_GoycoleaContardo).
:- use_module(libro_21310769_GoycoleaContardo).
:- use_module(prestamo_21310769_GoycoleaContardo).

%=============== Constructor ===================

% RF05: crearBiblioteca/10
% Descripción: Crea la biblioteca con parámetros de configuración y fecha inicial del sistema. maxLibrosUsuario: límite de libros simultáneos. diasMaxPrestamo: máximo de días permitidos. tasaMultaDiaria: monto por día de retraso. limiteDeudaMax: deuda máxima antes de suspensión. diasMaxRetraso: días de retraso antes de suspensión. fechaInicial: fecha de inicio del sistema.
% Parametros: crearBiblioteca(+Libros, +Usuarios, +Prestamos, +MaxLibros, +DiasMax, +TasaMulta, +LimiteDeuda, +DiasRetraso, +FechaInicial, -Biblioteca)
% Algoritmo: fuerza bruta
crearBiblioteca(Libros, Usuarios, Prestamos, MaxLibros, DiasMax, TasaMulta, LimiteDeuda, DiasRetraso, FechaInicial, Biblioteca) :-
    % Hacer verificaciones
    is_list(Libros),
    is_list(Usuarios),
    is_list(Prestamos),
    integer(MaxLibros), MaxLibros > 0,
    integer(DiasMax), DiasMax > 0,
    integer(TasaMulta), TasaMulta >= 0,
    integer(LimiteDeuda), LimiteDeuda >= 0,
    integer(DiasRetraso), DiasRetraso >= 0,
    % Verificar formato de fecha
    (string(FechaInicial) ; atom(FechaInicial)),
    % Crear biblioteca
    Biblioteca = [Libros, Usuarios, Prestamos, MaxLibros, DiasMax, TasaMulta, LimiteDeuda, DiasRetraso, FechaInicial].

%=============== Selectores ===================

% getBiblioteca{Elementos}/2
% Descripción: Obtiene la lista de {Elementos} de la biblioteca
% Parametros: getBiblioteca{Elementos}(+Biblioteca, -{Elementos})
% Algoritmo: fuerza bruta
getBibliotecaLibros([Libros, _, _, _, _, _, _, _, _], Libros).
getBibliotecaUsuarios([_, Usuarios, _, _, _, _, _, _, _], Usuarios).
getBibliotecaPrestamos([_, _, Prestamos, _, _, _, _, _, _], Prestamos).
getBibliotecaMaxLibros([_, _, _, MaxLibros, _, _, _, _, _], MaxLibros).
getBibliotecaDiasMax([_, _, _, _, DiasMax, _, _, _, _], DiasMax).
getBibliotecaTasaMulta([_, _, _, _, _, TasaMulta, _, _, _], TasaMulta).
getBibliotecaLimiteDeuda([_, _, _, _, _, _, LimiteDeuda, _, _], LimiteDeuda).
getBibliotecaDiasRetraso([_, _, _, _, _, _, _, DiasRetraso, _], DiasRetraso).

% RF11: getFecha/2
% Descripción: Obtiene la fecha actual del sistema desde la biblioteca.
% Parametros: getFecha(+Biblioteca, -Fecha)
% Algoritmo: fuerza bruta
getFecha([_, _, _, _, _, _, _, _, Fecha], Fecha).

%=============== Modificadores ===================

% setBiblioteca{Elementos}/3
% Descripción: Modifica la lista de {Elementos} de la biblioteca
% Parametros: setBiblioteca{Elementos}(+Biblioteca, +NUevos{Elementos}, -NuevaBiblioteca)
setBibliotecaLibros([_, Usuarios, Prestamos, MaxLibros, DiasMax, TasaMulta, LimiteDeuda, DiasRetraso, Fecha], NuevosLibros, 
                    [NuevosLibros, Usuarios, Prestamos, MaxLibros, DiasMax, TasaMulta, LimiteDeuda, DiasRetraso, Fecha]).
setBibliotecaUsuarios([Libros, _, Prestamos, MaxLibros, DiasMax, TasaMulta, LimiteDeuda, DiasRetraso, Fecha], NuevosUsuarios, 
                      [Libros, NuevosUsuarios, Prestamos, MaxLibros, DiasMax, TasaMulta, LimiteDeuda, DiasRetraso, Fecha]).
setBibliotecaPrestamos([Libros, Usuarios, _, MaxLibros, DiasMax, TasaMulta, LimiteDeuda, DiasRetraso, Fecha], NuevosPrestamos, 
                       [Libros, Usuarios, NuevosPrestamos, MaxLibros, DiasMax, TasaMulta, LimiteDeuda, DiasRetraso, Fecha]).
setBibliotecaFecha([Libros, Usuarios, Prestamos, MaxLibros, DiasMax, TasaMulta, LimiteDeuda, DiasRetraso, _], NuevaFecha, 
                   [Libros, Usuarios, Prestamos, MaxLibros, DiasMax, TasaMulta, LimiteDeuda, DiasRetraso, NuevaFecha]).

%=============== Otros ===================

% RF06: agregarLibro/3
% Descripción: Agrega un libro a la biblioteca. Si el ID ya existe, no se agrega y retorna la biblioteca sin cambios. Se permiten libros con mismo título/autor pero diferente ID. La posición donde se agrega el libro queda a criterio del estudiante.
% Parametros: agregarLibro(+BibliotecaIn, +Libro, -BibliotecaOut)
% Algoritmo: fuerza bruta
agregarLibro(BibliotecaIn, Libro, BibliotecaOut) :-
    getBibliotecaLibros(BibliotecaIn, Libros),
    getLibroId(Libro, IdLibro),
    % Verificar si el ID ya existe
    (buscarLibroPorId(Libros, IdLibro, _) ->
        % Si el ID ya existe no abregar
        BibliotecaOut = BibliotecaIn
    ;
        % Si el ID no existe se puede agregar
        append(Libros, [Libro], NuevosLibros),
        setBibliotecaLibros(BibliotecaIn, NuevosLibros, BibliotecaOut)
    ).

% RF07: registrarUsuario/3
% Descripción: Registra usuario en el sistema. Si el ID ya existe, no se registra y retorna la biblioteca sin cambios. Usuario inicia con deuda 0 y estado activo. El usuario debe agregarse AL FINAL de la lista de usuarios.
% Parametros: registrarUsuario(+BibliotecaIn, +Usuario, -BibliotecaOut)
% Algoritmo: fuerza bruta
registrarUsuario(BibliotecaIn, Usuario, BibliotecaOut) :-
    getBibliotecaUsuarios(BibliotecaIn, Usuarios),
    getUsuarioId(Usuario, IdUsuario),
    % Verificar si el ID ya existe
    (buscarUsuarioPorId(Usuarios, IdUsuario, _) ->
        % Si el ID ya existe no abregar
        BibliotecaOut = BibliotecaIn
    ;
        % Si el ID no existe se puede agregar
        append(Usuarios, [Usuario], NuevosUsuarios),
        setBibliotecaUsuarios(BibliotecaIn, NuevosUsuarios, BibliotecaOut)
    ).

% RF08: obtenerUsuario/3
% Descripción: Busca usuario por ID. Retorna false si no encuentra el usuario.
% Parametros: obtenerUsuario(+BibliotecaIn, +Id, -Usuario)
% Algoritmo: fuerza bruta

% Si existe el usuario retornarlo
obtenerUsuario(Biblioteca, Id, Usuario) :-
    getBibliotecaUsuarios(Biblioteca, Usuarios),
    buscarUsuarioPorId(Usuarios, Id, Usuario).
% Si no existe retornar false
obtenerUsuario(_, _, false) :- !.

% Auxiliar a obtenerUsuario
buscarUsuarioPorId(ListaUsuarios, IdBuscado, Usuario) :-
    member(Usuario, ListaUsuarios),
    getUsuarioId(Usuario, IdBuscado).

% RF09: buscarLibro/4
% Descripción: Busca un libro por "id", "titulo" o "autor". Para "titulo"/"autor": búsqueda parcial case-insensitive (todo en minúsculas). Para "id": búsqueda exacta. Si hay múltiples resultados, retorna el primero. Retorna false.
% Parametros: buscarLibro(+BibliotecaIn, +Criterio, +Valor, -LibroOut)
% Algoritmo: fuerza bruta

% Buscar por ID
buscarLibro(Biblioteca, "id", Valor, Libro) :-
    getBibliotecaLibros(Biblioteca, Libros),
    integer(Valor),
    buscarLibroPorId(Libros, Valor, Libro).

% Buscar por titulo
buscarLibro(Biblioteca, "titulo", Valor, Libro) :-
    getBibliotecaLibros(Biblioteca, Libros),
    buscarLibroPorTitulo(Libros, Valor, Libro), 
    !.

% Buscar por autor
buscarLibro(Biblioteca, "autor", Valor, Libro) :-
    getBibliotecaLibros(Biblioteca, Libros),
    buscarLibroPorAutor(Libros, Valor, Libro), 
    !.

% No existe ese libro
buscarLibro(_, _, _, false) :- 
    !.

% RF12: isLibroDisponible/2
% Descripción: Verifica si un libro está disponible (no prestado). Retorna false si el libro no existe o está prestado.
% Parametros: isLibroDisponible(+BibliotecaIn, +IdLibro)
% Algoritmo: fuerza bruta
isLibroDisponible(Biblioteca, IdLibro) :-
    getBibliotecaLibros(Biblioteca, Libros),
    buscarLibroPorId(Libros, IdLibro, Libro),
    isLibroDisponible(Libro).

% RF14: obtenerDeuda/2
% Descripción: Retorna la deuda acumulada del usuario.
% Parametros: obtenerDeuda(+Usuario, -Deuda)
% Algoritmo: fuerza bruta
obtenerDeuda(Usuario, Deuda) :-
    getUsuarioDeuda(Usuario, Deuda).

%=============== Funciones fecha ===================

% avanzarFecha/2
% Descripción: Avanza la fecha en un dia
% Parametros: avanzarFecha(+FechaActual, -FechaSiguiente)
% Algoritmo: fuerza bruta
avanzarFecha(FechaActual, FechaSiguiente) :-
    extraerDatosFecha(FechaActual, Dia, Mes),
    (Dia =:= 30 ->
        % Si el dia actual es 30
        % Cambiar de mes, nuevo dia es 1
        NuevoDia = 1,
        NuevoMes is Mes + 1
    ;
        % Si el dia actual no es 30
        % Es el mismo mes, pasar al dia siguiente
        NuevoDia is Dia + 1,
        NuevoMes = Mes
    ),
    formatearFecha(NuevoDia, NuevoMes, FechaSiguiente).

%=============== Prestamos de la biblioteca ===================

% RF18: tomarPrestamo/6
% Descripción: Usuario toma libro prestado. Verifica: 1) libro disponible, 2) usuario no suspendido, 3) no excede límite de libros, 4) días solicitados no exceden máximo, 5) deuda no excede límite. Si alguna verificación falla retorna false.
% Parametros: tomarPrestamo(+BibliotecaIn, +IdUsuario, +IdLibro, +Dias, +FechaActual, -BibliotecaOut)
% Algoritmo: fuerza bruta
tomarPrestamo(BibliotecaIn, IdUsuario, IdLibro, Dias, FechaActual, BibliotecaOut) :-
    % 1. Verificar que libro este disponible
    isLibroDisponible(BibliotecaIn, IdLibro),
    
    % 2. Obtener usuario y verificar que no esta suspendido
    obtenerUsuario(BibliotecaIn, IdUsuario, Usuario),
    Usuario \= false,
    \+ isUsuarioSuspendido(Usuario),
    
    % 3. Verificar que no haya alcanzado el maximo de libros
    getBibliotecaMaxLibros(BibliotecaIn, MaxLibros),
    getUsuarioLibros(Usuario, LibrosUsuario),
    length(LibrosUsuario, NumLibros),
    NumLibros < MaxLibros,
    
    % 4. Verificar que los dias solicitados no exedan el maximo
    getBibliotecaDiasMax(BibliotecaIn, DiasMax),
    Dias =< DiasMax,
    
    % 5. Verificar que la deuda no exceda el limite
    getBibliotecaLimiteDeuda(BibliotecaIn, LimiteDeuda),
    getUsuarioDeuda(Usuario, Deuda),
    Deuda =< LimiteDeuda,
    
    % Si se validaron todas las verificaciones, realizar el prestamo
    realizarPrestamo(BibliotecaIn, IdUsuario, IdLibro, Dias, FechaActual, BibliotecaOut).

% Auxiliar a tomar prestamo
realizarPrestamo(BibliotecaIn, IdUsuario, IdLibro, Dias, FechaActual, BibliotecaOut) :-
    % Generar ID
    getBibliotecaPrestamos(BibliotecaIn, Prestamos),
    generarIdPrestamo(Prestamos, NuevoIdPrestamo),
    
    % Crear el prestamo
    crearPrestamo(NuevoIdPrestamo, IdUsuario, IdLibro, FechaActual, Dias, NuevoPrestamo),
    
    % Agregar el prestamo a la lista de prestamos
    append(Prestamos, [NuevoPrestamo], NuevosPrestamos),
    setBibliotecaPrestamos(BibliotecaIn, NuevosPrestamos, BibliotecaTemp1),
    
    % Marcar libro como no disponible
    marcarLibroNoDisponible(BibliotecaTemp1, IdLibro, BibliotecaTemp2),
    
    % Agregar el libro a la lista de libros del usuario
    agregarLibroAUsuario(BibliotecaTemp2, IdUsuario, IdLibro, BibliotecaOut).

% Genera ID para el prestamo

% Primer prestamo tiene ID 1
generarIdPrestamo([], 1).

% Prestamos siguientes se le suma 1 al mayor ID
generarIdPrestamo(Prestamos, NuevoId) :-
    maplist(getPrestamoId, Prestamos, Ids),
    max_list(Ids, MaxId),
    NuevoId is MaxId + 1.

%=============== Modificadores ===================

% Marcar libro como no disponible
marcarLibroNoDisponible(BibliotecaIn, IdLibro, BibliotecaOut) :-
    getBibliotecaLibros(BibliotecaIn, Libros),
    modificarDisponibilidadLibro(Libros, IdLibro, false, NuevosLibros),
    setBibliotecaLibros(BibliotecaIn, NuevosLibros, BibliotecaOut).

% Modifica la disponibilidad de un libro
modificarDisponibilidadLibro([Libro|Resto], IdLibro, NuevaDisponibilidad, [LibroModificado|Resto]) :-
    getLibroId(Libro, IdLibro), !,
    setLibroDisponible(Libro, NuevaDisponibilidad, LibroModificado).
modificarDisponibilidadLibro([Libro|Resto], IdLibro, NuevaDisponibilidad, [Libro|NuevoResto]) :-
    modificarDisponibilidadLibro(Resto, IdLibro, NuevaDisponibilidad, NuevoResto).

% Agregar un libro a la lista de libros del usuario
agregarLibroAUsuario(BibliotecaIn, IdUsuario, IdLibro, BibliotecaOut) :-
    getBibliotecaUsuarios(BibliotecaIn, Usuarios),
    modificarLibrosUsuario(Usuarios, IdUsuario, IdLibro, agregar, NuevosUsuarios),
    setBibliotecaUsuarios(BibliotecaIn, NuevosUsuarios, BibliotecaOut).

% Modifica la lista de libros del usuario
modificarLibrosUsuario([Usuario|Resto], IdUsuario, IdLibro, Operacion, [UsuarioModificado|Resto]) :-
    getUsuarioId(Usuario, IdUsuario), !,
    (Operacion = agregar ->
        agregarLibroUsuario(Usuario, IdLibro, UsuarioModificado)
    ;
        removerLibroUsuario(Usuario, IdLibro, UsuarioModificado)
    ).
modificarLibrosUsuario([Usuario|Resto], IdUsuario, IdLibro, Operacion, [Usuario|NuevoResto]) :-
    modificarLibrosUsuario(Resto, IdUsuario, IdLibro, Operacion, NuevoResto).
        % Si el dia actual no es 30
        % Es el mismo mes, pasar al dia siguiente
        NuevoDia is Dia + 1,
        NuevoMes = Mes
    ),
    formatearFecha(NuevoDia, NuevoMes, FechaSiguiente).

% RF19: devolverLibro/5
% Descripción: Devuelve un libro y procesa multas. Calcula multa por retraso, suma la deuda del usuario, marca el libro disponible. Si la deuda total excede el límite, se suspende al usuario automáticamente. Usuario suspendido SÍ puede devolver.
% Parametros: devolverLibro(+BibliotecaIn, +IdUsuario, +IdLibro, +FechaActual, -BibliotecaOut)
% Algoritmo: fuerza bruta
devolverLibro(BibliotecaIn, IdUsuario, IdLibro, FechaActual, BibliotecaOut) :-
    % Buscar el prestamo del usuario correspondiente al ID del libro
    getBibliotecaPrestamos(BibliotecaIn, Prestamos),
    buscarPrestamoActivo(Prestamos, IdUsuario, IdLibro, Prestamo),
    
    % Calcular multa por retraso
    getBibliotecaTasaMulta(BibliotecaIn, TasaMulta),
    calcularMulta(Prestamo, FechaActual, TasaMulta, Multa),
    
    % Actualizar deuda del usuario
    actualizarDeudaUsuario(BibliotecaIn, IdUsuario, Multa, BibliotecaTemp1),
    
    % Marcar prestamo como completado
    marcarPrestamoCompletado(BibliotecaTemp1, Prestamo, BibliotecaTemp2),
    
    % Marcar libro como disponible
    marcarLibroDisponible(BibliotecaTemp2, IdLibro, BibliotecaTemp3),
    
    % Remover libro de la lista del usuario
    removerLibroDeUsuario(BibliotecaTemp3, IdUsuario, IdLibro, BibliotecaTemp4),
    
    % Verificar y suspender al usuario en caso de que se rompio alguna regla
    (debeSuspenderse(BibliotecaTemp4, IdUsuario, FechaActual) ->
        suspenderUsuario(BibliotecaTemp4, IdUsuario, BibliotecaOut)
    ;
        BibliotecaOut = BibliotecaTemp4
    ).

% RF20: debeSuspenderse/3
% Descripción: Verifica si el usuario debe suspenderse automáticamente. Criterios: 1) deuda > límite máximo, O 2) tiene algún libro con retraso > días máximos permitidos. Si el usuario debe ser suspendido retorna true caso contrario false.
% Parametros: debeSuspenderse(+Biblioteca, +IdUsuario, +FechaActual)
% Algoritmo: fuerza bruta
debeSuspenderse(Biblioteca, IdUsuario, FechaActual) :-
    % 1. Deuda excede el limite
    obtenerUsuario(Biblioteca, IdUsuario, Usuario),
    Usuario \= false,
    getBibliotecaLimiteDeuda(Biblioteca, LimiteDeuda),
    getUsuarioDeuda(Usuario, Deuda),
    (Deuda > LimiteDeuda ->
        true
    ;
        % 2. Retraso en devolución del libro por sobre lo permitido
        getBibliotecaDiasRetraso(Biblioteca, DiasMaxRetraso),
        tieneRetrasoExcesivo(Biblioteca, IdUsuario, FechaActual, DiasMaxRetraso)
    ).

% RF21: suspenderUsuario/3
% Descripción: Suspende al usuario manualmente cambiando su estado a true. Si el usuario se encuentra suspendido entonces el usuario se mantiene intacto en la biblioteca de salida.
% Parametros: suspenderUsuario(+BibliotecaIn, +IdUsuario, -BibliotecaOut)
% Algoritmo: fuerza bruta
suspenderUsuario(BibliotecaIn, IdUsuario, BibliotecaOut) :-
    getBibliotecaUsuarios(BibliotecaIn, Usuarios),
    modificarSuspensionUsuario(Usuarios, IdUsuario, true, NuevosUsuarios),
    setBibliotecaUsuarios(BibliotecaIn, NuevosUsuarios, BibliotecaOut).

%=============== Auxiliares ===================

% Buscar prestamo activo especifico

% Se encontro el prestamo que se busca
buscarPrestamoActivo([Prestamo|_], IdUsuario, IdLibro, Prestamo) :-
    getPrestamoIdUsuario(Prestamo, IdUsuario),
    getPrestamoIdLibro(Prestamo, IdLibro),
    isPrestamoActivo(Prestamo), 
    !.

% Seguir buscando el prestamo que se busca en el resto de la lista
buscarPrestamoActivo([_|Resto], IdUsuario, IdLibro, Prestamo) :-
    buscarPrestamoActivo(Resto, IdUsuario, IdLibro, Prestamo).

% Actualizar deuda del usuario sumandole la multa correspondiente

% Actualizar deuda usuario
actualizarDeudaUsuario(BibliotecaIn, IdUsuario, Multa, BibliotecaOut) :-
    getBibliotecaUsuarios(BibliotecaIn, Usuarios),
    actualizarDeudaEnLista(Usuarios, IdUsuario, Multa, NuevosUsuarios),
    setBibliotecaUsuarios(BibliotecaIn, NuevosUsuarios, BibliotecaOut).

% Actualizar deuda usuario en lista de usuarios
actualizarDeudaEnLista([Usuario|Resto], IdUsuario, Multa, [UsuarioModificado|Resto]) :-
    getUsuarioId(Usuario, IdUsuario), !,
    getUsuarioDeuda(Usuario, DeudaActual),
    NuevaDeuda is DeudaActual + Multa,
    setUsuarioDeuda(Usuario, NuevaDeuda, UsuarioModificado).
actualizarDeudaEnLista([Usuario|Resto], IdUsuario, Multa, [Usuario|NuevoResto]) :-
    actualizarDeudaEnLista(Resto, IdUsuario, Multa, NuevoResto).

% Marcar prestamo como completado
marcarPrestamoCompletado(BibliotecaIn, Prestamo, BibliotecaOut) :-
    getBibliotecaPrestamos(BibliotecaIn, Prestamos),
    getPrestamoId(Prestamo, IdPrestamo),
    marcarPrestamoInactivoEnLista(Prestamos, IdPrestamo, NuevosPrestamos),
    setBibliotecaPrestamos(BibliotecaIn, NuevosPrestamos, BibliotecaOut).

% Marcar prestamo como inactivo
marcarPrestamoInactivoEnLista([Prestamo|Resto], IdPrestamo, [PrestamoModificado|Resto]) :-
    getPrestamoId(Prestamo, IdPrestamo), !,
    setPrestamoActivo(Prestamo, false, PrestamoModificado).
marcarPrestamoInactivoEnLista([Prestamo|Resto], IdPrestamo, [Prestamo|NuevoResto]) :-
    marcarPrestamoInactivoEnLista(Resto, IdPrestamo, NuevoResto).

% Marcar un libro como disponible
marcarLibroDisponible(BibliotecaIn, IdLibro, BibliotecaOut) :-
    getBibliotecaLibros(BibliotecaIn, Libros),
    modificarDisponibilidadLibro(Libros, IdLibro, true, NuevosLibros),
    setBibliotecaLibros(BibliotecaIn, NuevosLibros, BibliotecaOut).

% Quitar libro de la lista de libros de un usuario
removerLibroDeUsuario(BibliotecaIn, IdUsuario, IdLibro, BibliotecaOut) :-
    getBibliotecaUsuarios(BibliotecaIn, Usuarios),
    modificarLibrosUsuario(Usuarios, IdUsuario, IdLibro, remover, NuevosUsuarios),
    setBibliotecaUsuarios(BibliotecaIn, NuevosUsuarios, BibliotecaOut).

% Verificar si usuario esta retrasado en la devolución de un libro
tieneRetrasoExcesivo(Biblioteca, IdUsuario, FechaActual, DiasMaxRetraso) :-
    getBibliotecaPrestamos(Biblioteca, Prestamos),
    prestamosActivosUsuario(Prestamos, IdUsuario, PrestamosUsuario),
    member(Prestamo, PrestamosUsuario),
    obtenerFechaVencimiento(Prestamo, FechaVencimiento),
    calcularDiasRetraso(FechaVencimiento, FechaActual, DiasRetraso),
    DiasRetraso > DiasMaxRetraso, 
    !.

% Obtener prestamos activos de un usuario
prestamosActivosUsuario([], _, []).
prestamosActivosUsuario([Prestamo|Resto], IdUsuario, [Prestamo|PrestamosUsuario]) :-
    getPrestamoIdUsuario(Prestamo, IdUsuario),
    isPrestamoActivo(Prestamo), 
    !,
    prestamosActivosUsuario(Resto, IdUsuario, PrestamosUsuario).
prestamosActivosUsuario([_|Resto], IdUsuario, PrestamosUsuario) :-
    prestamosActivosUsuario(Resto, IdUsuario, PrestamosUsuario).

% Modifica estado de suspendido de un usuario
modificarSuspensionUsuario([Usuario|Resto], IdUsuario, NuevoEstado, [UsuarioModificado|Resto]) :-
    getUsuarioId(Usuario, IdUsuario), 
    !,
    setUsuarioSuspendido(Usuario, NuevoEstado, UsuarioModificado).
modificarSuspensionUsuario([Usuario|Resto], IdUsuario, NuevoEstado, [Usuario|NuevoResto]) :-
    modificarSuspensionUsuario(Resto, IdUsuario, NuevoEstado, NuevoResto).

% RF22: renovarPrestamo/5
% Descripción: Renueva préstamo agregando días extra. Verifica: 1) préstamo existe y está activo, 2) usuario no suspendido, 3) no hay retraso actual, 4) días totales no exceden máximo. Si falla alguna verificación, retorna false.
% Parametros: renovarPrestamo(+BibliotecaIn, +IdPrestamo, +DiasExtra, +FechaActual, -BibliotecaOut)
% Algoritmo: fuerza bruta
renovarPrestamo(BibliotecaIn, IdPrestamo, DiasExtra, FechaActual, BibliotecaOut) :-
    % 1. Verificar que el prestamo exista y que este activo
    getBibliotecaPrestamos(BibliotecaIn, Prestamos),
    buscarPrestamoPorId(Prestamos, IdPrestamo, Prestamo),
    isPrestamoActivo(Prestamo),
    
    % 2. Verificar que el usuario no este suspendido
    getPrestamoIdUsuario(Prestamo, IdUsuario),
    obtenerUsuario(BibliotecaIn, IdUsuario, Usuario),
    Usuario \= false,
    \+ isUsuarioSuspendido(Usuario),
    
    % 3. Verificar que la devolución del libro no este atrasada
    obtenerFechaVencimiento(Prestamo, FechaVencimiento),
    calcularDiasRetraso(FechaVencimiento, FechaActual, DiasRetraso),
    DiasRetraso =:= 0,
    
    % 4. Verificar que los dias pedidos no superen el maximo permitido
    getPrestamoDiasSolicitados(Prestamo, DiasActuales),
    DiasTotal is DiasActuales + DiasExtra,
    getBibliotecaDiasMax(BibliotecaIn, DiasMax),
    DiasTotal =< DiasMax,
    
    % Si se validaron todas las verificaciones, extender el prestamo
    actualizarDiasPrestamo(BibliotecaIn, IdPrestamo, DiasTotal, BibliotecaOut).

% RF23: pagarDeuda/4
% Descripción: Usuario paga monto para reducir deuda. Para reactivarse debe: pagar TODA la deuda (deuda=0) Y no tener libros con retraso excesivo. Si paga parcialmente, reduce deuda pero sigue suspendido.
% Parametros: pagarDeuda(+BibliotecaIn, +IdUsuario, +Monto, -BibliotecaOut)
% Algoritmo: fuerza bruta
pagarDeuda(BibliotecaIn, IdUsuario, Monto, BibliotecaOut) :-
    % Reducir la deuda del usuario
    getBibliotecaUsuarios(BibliotecaIn, Usuarios),
    pagarDeudaEnLista(Usuarios, IdUsuario, Monto, NuevosUsuarios),
    setBibliotecaUsuarios(BibliotecaIn, NuevosUsuarios, BibliotecaTemp),
    
    % Verificar si el usuario puede reactivarse
    obtenerUsuario(BibliotecaTemp, IdUsuario, UsuarioActualizado),
    getUsuarioDeuda(UsuarioActualizado, DeudaFinal),
    getFecha(BibliotecaTemp, FechaActual),
    
    getBibliotecaDiasRetraso(BibliotecaTemp, DiasMaxRetraso),
    (DeudaFinal =:= 0, \+ tieneRetrasoExcesivo(BibliotecaTemp, IdUsuario, FechaActual, DiasMaxRetraso) ->
        % Si la deuda es 0 y no tiene libros con retraso, reactivar al usuario
        reactivarUsuario(BibliotecaTemp, IdUsuario, BibliotecaOut)
    ;
        % Si no, mantener suspendido
        BibliotecaOut = BibliotecaTemp
    ).
