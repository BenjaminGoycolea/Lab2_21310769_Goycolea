% Exportar predicados
:- module(usuario_21310769_GoycoleaContardo, [
    crearUsuario/3,
    getUsuarioId/2,
    getUsuarioNombre/2,
    getUsuarioDeuda/2,
    getUsuarioLibros/2,
    isUsuarioSuspendido/1,
    setUsuarioDeuda/3,
    setUsuarioSuspendido/3,
    agregarLibroUsuario/3,
    removerLibroUsuario/3
]).

%=============== Constructor ===================

% RF02: crearUsuario/3
% Descripción: Crea un usuario con deuda inicial 0 y estado activo (no suspendido).
% Parametros: crearUsuario(+Id, +Nombre, -Usuario)
% Algoritmo: fuerza bruta
crearUsuario(Id, Nombre, Usuario) :-
    % Verificaciones
    integer(Id),
    (string(Nombre) ; atom(Nombre)),
    % Usuario = [Id, Nombre, Deuda=0, LibrosPrestados=[], Suspendido=false]
    Usuario = [Id, Nombre, 0, [], false].

%=============== Funciones de Pertenencia ===================

% esUsuario/1
% Descripción: Verifica si elemento es un usuario valido
% Parametros: esUsuario(+SupuestoUsuario) -Booleano
% Algoritmo: fuerza bruta
esUsuario([Id, Nombre, Deuda, Libros, Suspendido]) :-
    integer(Id),
    (atom(Nombre) ; string(Nombre)),
    integer(Deuda),
    Deuda >= 0,
    is_list(Libros),
    (Suspendido = true ; Suspendido = false).

%=============== Selectores ===================

% getUsuario{Elemento}/2
% Descripción: Obtiene el {Elemento} del usuario
% Parametros: getUsuario{Elemento}(+Usuario, -{Elemento})
% Algoritmo: fuerza bruta
getUsuarioId([Id, _, _, _, _], Id).
getUsuarioNombre([_, Nombre, _, _, _], Nombre).
getUsuarioDeuda([_, _, Deuda, _, _], Deuda).
getUsuarioLibros([_, _, _, Libros, _], Libros).


% RF13: isUsuarioSuspendido/1
% Descripción: Verifica estado de suspensión del usuario. Un usuario está suspendido si su campo suspendido es #t.
% Parametros: isUsuarioSuspendido(+Usuario) -Booleano
% Algoritmo: fuerza bruta
isUsuarioSuspendido([_, _, _, _, true]).

%=============== Modificadores ===================

% setUsuario{Elemento}/3
% Descripción: Modifica {Elemento} del usuario
% Parametros: setUsuario{Elemento}(+Usuario, +Nuevo{Elemento}, -UsuarioModificado)
% Algoritmo: fuerza bruta
setUsuarioDeuda([Id, Nombre, _, Libros, Suspendido], NuevaDeuda, [Id, Nombre, NuevaDeuda, Libros, Suspendido]) :-
    integer(NuevaDeuda),
    NuevaDeuda >= 0.
setUsuarioSuspendido([Id, Nombre, Deuda, Libros, _], NuevoSuspendido, [Id, Nombre, Deuda, Libros, NuevoSuspendido]) :-
    (NuevoSuspendido = true ; NuevoSuspendido = false).
agregarLibroUsuario([Id, Nombre, Deuda, Libros, Suspendido], IdLibro, [Id, Nombre, Deuda, NuevosLibros, Suspendido]) :-
    integer(IdLibro),
    append(Libros, [IdLibro], NuevosLibros).
removerLibroUsuario([Id, Nombre, Deuda, Libros, Suspendido], IdLibro, [Id, Nombre, Deuda, NuevosLibros, Suspendido]) :-
    integer(IdLibro),
    delete(Libros, IdLibro, NuevosLibros).
