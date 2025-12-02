
:- module(libro_21310769_GoycoleaContardo, [
    crearLibro/4,
]).

% RF03: crearLibro/4
% Descripción: Crea un libro con id, título y autor. Título y autor se convierten en minúsculas al crear
% Dominio: id (int) X titulo (string) X autor (string) X libro (Libro)
% Uso: crearLibro(+Id, +Titulo, +Autor, -Libro)
crearLibro(Id, Titulo, Autor, Libro) :-
    % Hacer verificaciones
    integer(Id),
    (string(Titulo) ; atom(Titulo)),
    (string(Autor) ; atom(Autor)),
    % Convertir a minuscula
    downcase_atom(Titulo, TituloLower),
    downcase_atom(Autor, AutorLower),
    % Crear libro: [Id, Titulo, Autor, Disponible=true]
    Libro = [Id, TituloLower, AutorLower, true].
