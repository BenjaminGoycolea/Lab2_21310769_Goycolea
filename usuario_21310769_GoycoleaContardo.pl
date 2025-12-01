:- module(usuario_21310769_GoycoleaContardo, [
    crearUsuario/3,
]).

% RF02: crearUsuario/3
% Descripción: Crea un usuario con deuda inicial 0 y estado activo (no suspendido).
% Parametros: id (int) X nombre (string) X usuario (Usuario)
% Uso: jugador(+Id, +Nombre, -Usuario)
crearUsuario(Id, Nombre, Usuario) :-
    % Verificar condiciones
    integer(Id),
    (string(Nombre) ; atom(Nombre)),
    downcase_atom(Nombre, NombreLower),
    Usuario = [Id, NombreLower, 0, [], false].
