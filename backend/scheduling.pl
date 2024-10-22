% Declarar predicados dinámicos
:- dynamic professor/3.
:- dynamic course/4.

% Rooms (Salas disponibles)
room(lab1, 20, lab).
room(lab2, 25, lab).
room(lab3, 30, lab).
room(moviles, 20, normal).
room(miniauditorio, 40, normal).

% Time slots available for classes (Intervalos de tiempo disponibles para clases)
available_time_slot(monday, 7, 1130).
available_time_slot(monday, 1230, 1600).
available_time_slot(tuesday, 7, 1130).
available_time_slot(tuesday, 1230, 1600).
available_time_slot(wednesday, 7, 1130).
available_time_slot(wednesday, 1230, 1600).
available_time_slot(thursday, 7, 1130).
available_time_slot(thursday, 1230, 1600).
available_time_slot(friday, 7, 1130).
available_time_slot(friday, 1230, 1600).

% Predicado para determinar si un semestre es par
is_even(Semester) :-
    Semester mod 2 =:= 0.

% Predicado para determinar si un semestre es impar
is_odd(Semester) :-
    Semester mod 2 =:= 1.

% Reglas para determinar disponibilidad de un profesor
available_professor(Professor, Day, Start, End) :-
    professor(Professor, AvailableHours, _),
    member([Day, Start, End], AvailableHours).

% Reglas para determinar disponibilidad de una sala
available_room(Room, RoomType) :-
    room(Room, _, RoomType).

% Verifica si un profesor puede enseñar un curso
can_teach(Professor, Course) :-
    professor(Professor, _, Courses),
    member(Course, Courses).

% Valida que el horario esté dentro de las franjas disponibles
valid_time_slot(Day, Start, End) :-
    available_time_slot(Day, AvailableStart, AvailableEnd),
    Start >= AvailableStart,
    End =< AvailableEnd.

% Encuentra posibles horarios para cursos de semestres pares o impares
find_schedule_for_semester_parity(Parity, Schedule) :-
    % Obtener cursos que coinciden con la paridad (pares o impares)
    findall(CourseName,
        (course(CourseName, _, _, Semester),
         ( (Parity = even, is_even(Semester)) ;
           (Parity = odd, is_odd(Semester)) )),
        Courses),
    % Encontrar horarios para esos cursos
    find_schedule_for_courses(Courses, Schedule).

% Encuentra un horario posible para una lista de cursos sin preocuparse por intentos
find_schedule_for_courses(Courses, Schedule) :-
    find_schedule_for_courses_helper(Courses, [], Schedule).

% Asigna horarios a los cursos seleccionados, salta un curso si genera conflicto o ya está asignado
find_schedule_for_courses_helper([], Acc, Acc) :- !.  
find_schedule_for_courses_helper([Course | Rest], Acc, Schedule) :-
    % Verificar si el curso ya ha sido asignado previamente
    (   member([Course, _, _, _, _, _], Acc)
    ->  writef('Curso ya asignado: %w, saltando al siguiente\n', [Course]),
        find_schedule_for_courses_helper(Rest, Acc, Schedule)
    ;   % Intentar asignar el curso
        course(Course, RoomType, _, _),
        can_teach(Professor, Course),
        available_professor(Professor, Day, Start, End),
        valid_time_slot(Day, Start, End),
        available_room(Room, RoomType),
        \+ conflict(Course, Professor, Room, Day, Start, End, Acc)
    ->  % Si no hay conflicto, asignamos el curso
        writef('Asignando curso: %w con profesor %w en sala %w el %w de %w a %w\n',
               [Course, Professor, Room, Day, Start, End]),
        NewAcc = [[Course, Professor, Room, Day, Start, End] | Acc],
        find_schedule_for_courses_helper(Rest, NewAcc, Schedule)
    ;   % Si hay conflicto, se salta el curso
        writef('Conflicto encontrado para el curso: %w, saltando al siguiente curso\n', [Course]),
        find_schedule_for_courses_helper(Rest, Acc, Schedule)
    ).

% Reglas para detectar conflictos entre horarios
conflict(_, Professor, _, Day, Start1, End1, Schedule) :-
    member([_, Professor, _, Day, Start2, End2], Schedule),
    overlap(Start1, End1, Start2, End2).

conflict(_, _, Room, Day, Start1, End1, Schedule) :-
    member([_, _, Room, Day, Start2, End2], Schedule),
    overlap(Start1, End1, Start2, End2).

% Verifica si dos franjas horarias se solapan
overlap(Start1, End1, Start2, End2) :-
    Start1 < End2,
    Start2 < End1.
