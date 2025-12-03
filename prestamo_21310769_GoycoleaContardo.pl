
:- module(prestamo_21310769_GoycoleaContardo, [
    crearPrestamo/6,
]).

% RF04: crearPrestamo/6
% Descripción: Crea un préstamo. Los días solicitados no deben exceder el máximo permitido por el sistema
% Dominio: id (int) X idUsuario (int) X idLibro (int) X fechaPrestamo (string) X diasSolicitados (int) X prestamo (Prestamo)
% Uso: crearPrestamo(+Id, +IdUsuario, +IdLibro, +FechaPrestamo, +DiasSolicitados, -Prestamo)
crearPrestamo(Id, IdUsuario, IdLibro, FechaPrestamo, DiasSolicitados, Prestamo) :-
    % Verificaciones
    integer(Id),
    integer(IdUsuario),
    integer(IdLibro),
    integer(DiasSolicitados),
    DiasSolicitados > 0,
    % Verificar formato
    (string(FechaPrestamo) ; atom(FechaPrestamo)),
    % Crear prestamo: [Id, IdUsuario, IdLibro, FechaPrestamo, DiasSolicitados, Activo=true]
    Prestamo = [Id, IdUsuario, IdLibro, FechaPrestamo, DiasSolicitados, true].
