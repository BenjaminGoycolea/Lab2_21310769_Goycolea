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
