# Proyecto_IA
Utilizando la herramienta prolog, crear una base de conocimiento para los deportes (Surf)
%Equipamiento
equipo(tabla).
equipo(cera).
equipo(traje_de_neopreno).
equipo(quillas).
equipo(inventos).
equipo(parafina).
equipo(fundas).
equipo(sujetador_de_tabla).
equipo(gorra_de_surf).
equipo(gafas_de_sol).
equipo(reloj_a_prueba_de_agua).
equipo(casco_de_proteccion).
equipo(zapatos_de_neopreno).
equipo(cuerda_de_seguridad).
equipo(cera_de_base).
equipo(crema_solar).
equipo(bolsa_estanca).
equipo(rack_de_techo).
equipo(juego_de_quillas_de_repuesto).
equipo(licra_de_proteccion).
equipo(leash).
equipo(alerones).
equipo(tabla_de_entrenamiento).
equipo(manta_de_surf).
equipo(waxcomb).
equipo(gorra_de_neopreno).
equipo(calcetines_de_neopreno).

%Niveles_de_flexibilidad_del_traje
flexibilidad(neopreno, alta).
flexibilidad(lycra, media).
flexibilidad(hibrido, alta).


%Tecnicas_de_surf
tecnica(remar).
tecnica(levantarse).
tecnica(leer_oleaje).
tecnica(giros).
tecnica(cutbacks).
tecnica(aereos).
tecnica(tuberide).
tecnica(bottom_turn).
tecnica(floater).
tecnica(el_360).
tecnica(reentry).
tecnica(snap).
tecnica(float_and_spin).
tecnica(backside_barrel).
tecnica(big_wave_riding).
tecnica(duck_dive).
tecnica(turtle_roll).
tecnica(pig_dog).
tecnica(cheater_five).



%Tipos_de_olas
tipo_ola(pequena).
tipo_ola(mediana).
tipo_ola(grande).
tipo_ola(tubo).
tipo_ola(limpia).
tipo_ola(choppy).
tipo_ola(lenta).
tipo_ola(rapida).
tipo_ola(barrel).
tipo_ola(point_break).
tipo_ola(beach_break).
tipo_ola(reef_break).
tipo_ola(marea_vacilante).
tipo_ola(derecha).
tipo_ola(izquierda).
tipo_ola(hueca).
tipo_ola(fat).
tipo_ola(larga).
tipo_ola(corta).
tipo_ola(ondulada).
tipo_ola(espesa).
tipo_ola(ondulante).
tipo_ola(estacionaria).
tipo_ola(cerrada).
tipo_ola(abierta).


%Geografia
caracteristica_playa(profundidad_del_agua).
caracteristica_playa(configuracion_fondo).
caracteristica_playa(zonas_peligrosas).
caracteristica_playa(zonas_acceso).
caracteristica_playa(tamanio_olas).
caracteristica_playa(forma_olas).
caracteristica_playa(mareas).
caracteristica_playa(orientacion_viento).
caracteristica_playa(temperatura_agua).
caracteristica_playa(cantidad_sol).
caracteristica_playa(corriente).
caracteristica_playa(altura_olas).
caracteristica_playa(frecuencia_olas).
caracteristica_playa(rompiente).
caracteristica_playa(direccion_viento).

:- discontiguous termino_tecnico/1.

%Terminologia
termino_tecnico(tube).
termino_tecnico(wipeout).
termino_tecnico(duck_dive).
termino_tecnico(cutback).
termino_tecnico(surfista_goofy).
termino_tecnico(surfista_regular).
termino_tecnico(inside).
termino_tecnico(outside).
termino_tecnico(air).
termino_tecnico(barrel).
termino_tecnico(bottom_turn).
termino_tecnico(drop_in).
termino_tecnico(face).
termino_tecnico(floater).
termino_tecnico(kook).
termino_tecnico(lip).
termino_tecnico(nose).
termino_tecnico(off_the_lip).
termino_tecnico(snap).
termino_tecnico(tail).
termino_tecnico(wax).


%Temperatura_del_agua_en_grados_Celsius
temperatura_agua(bondi, 20).
temperatura_agua(manly, 18).
temperatura_agua(bells_beach, 16).
temperatura_agua(snapper_rocks, 22).


%Condiciones_de_surf
condiciones(bondi, soleado).
condiciones(manly, nublado).
condiciones(bells_beach, ventoso).
condiciones(snapper_rocks, tormenta).


%Tamanio_maximo_de_olas_por_nivel_de_habilidad
tamanio_maximo(principiante, pequena).
tamanio_maximo(intermedio, mediana).
tamanio_maximo(avanzado, grande).
tamanio_maximo(experto, gigante).


%Duracion_promedio_de_las_olas_en_segundos
duracion_ola(pequena, 5).
duracion_ola(mediana, 10).
duracion_ola(grande, 15).
duracion_ola(gigante, 20).

%Beneficios_fisicos
beneficio_fisico(aumento_fuerza).
beneficio_fisico(mejora_equilibrio).
beneficio_fisico(acondicionamiento_cardiovascular).
beneficio_fisico(quema_calorias).


%Competiciones_de_surf

%Series_Clasificatorias_Mundiales
competencia(series_clasificatorias_mundiales).
sede(series_clasificatorias_mundiales, varias).
fecha(series_clasificatorias_mundiales, 2023).
ganador(series_clasificatorias_mundiales, sam_smith).


%Historia_del_surf
historia(origen, 'El surf moderno se origino en Hawai en el siglo XIX').
historia(figuras_destacadas, 'Duke Kahanamoku fue un destacado surfista y embajador del deporte en el siglo XX').
historia(aparicion_competencias, 'En 1928 se llevo a cabo la primera competencia de surf en Hawai').

%condiciones_ideales_para_surfear_segun_el_nivel_de_habilidad
condiciones_ideales(principiante, soleado).
condiciones_ideales(intermedio, soleado).
condiciones_ideales(avanzado, soleado).
condiciones_ideales(experto, soleado).

%Hechos_sobre_puntajes
puntaje(mala, 0).
puntaje(mediocre, 2).
puntaje(buena, 8). 
puntaje(excelente,10).


%Hechos_sobre_jueces
juez(juez1).
juez(juez2).
juez(juez3).
juez(juez4).
juez(juez5).
juez(juez6).
juez(juez7).
juez(juez8).
juez(juez9).
juez(juez10).


%Hechos_sobre_los_lugares_en_el_surf
lugar(primer_lugar).
lugar(segundo_lugar).
lugar(tercer_lugar).


%Reglaparaverificarsiunequipoesnecesario
necesario(Equipo) :-
  equipo(Equipo).


%Regla_para_verificar_si_dos_equipos_son_incompatibles
incompatible(Equipo1, Equipo2) :-
  (Equipo1 = tabla, Equipo2 = quillas) ; %No_se_pueden_usar_quillas_sin_tabla
  (Equipo1 = traje_de_neopreno, Equipo2 = crema_solar) ; %La_crema_puede_danar_el_neopreno
  (Equipo1 = leash, Equipo2 = sujetador_de_tabla) ; %Ambos_son_para_mantener_la_tabla_cerca_del_surfista
  (Equipo1 = gorra_de_surf, Equipo2 = gafas_de_sol) ; %Ambos_protegen_la_cabeza_y_la_cara
  (Equipo1 = gorra_de_neopreno, Equipo2 = gafas_de_sol) ; %Ambos_protegen_la_cabeza_y_la_cara
  (Equipo1 = zapatos_de_neopreno, Equipo2 = calcetines_de_neopreno). %No_es_necesario_usar_ambos_a_la_vez

%Regla_para_verificar_la_flexibilidad_de_un_equipo
flexible(Equipo) :-
  flexibilidad(Equipo, alta).

%Regla_para_verificar_si_la_historia_del_surf 
historia_origen_hawai(Historia) :-
    historia(origen, Historia),
    string_lower(Historia, Historia_lower),
    sub_string(Historia_lower, _, _, _, "hawai"),
    (
        sub_string(Historia_lower, _, _, _, "siglo xix")
        ;
        sub_string(Historia_lower, _, _, _, "siglo 19")
    ).
%Regla_para_verificar_si_Duke_Kahanamoku_fue_una_figura_destacada 
historia_figuras_destacadas(Historia) :-
    historia(figuras_destacadas, Historia),
    string_lower(Historia, Historia_lower),
    sub_string(Historia_lower, _, _, _, "duke kahanamoku").


%Regla_para_verificar_si_aparecieron_competencias_de_surf 
historia_competencias(Historia) :-
    historia(aparicion_competencias, Historia),
    string_lower(Historia, Historia_lower),
    sub_string(Historia_lower, _, _, _, "hawai"),
    sub_string(Historia_lower, _, _, _, "1928").


%Regla_para_verificar_si_dos_equipos_son_compatibles_en_terminos_de_flexibilidad
compatibles(Equipo1, Equipo2) :-
  (flexibilidad(Equipo1, alta), flexibilidad(Equipo2, alta)) ; %Dos_equipos_altamente_flexibles_son_compatibles
  (flexibilidad(Equipo1, media), flexibilidad(Equipo2, media)) ; %Dos_equipos_medianamente_flexibles_son_compatibles
  (flexibilidad(Equipo1, alta), flexibilidad(Equipo2, media)) ; %Un_equipo_altamente_flexible_y_uno_medianamente_flexible_son_compatibles
  (flexibilidad(Equipo1, media), flexibilidad(Equipo2, alta)). %Lo_mismo_en_el_orden_inverso

%Regla_para_verificar_si_una_tecnica_es_dificil
dificil(Tecnica) :-
  avanzada(Tecnica) ; %Las_tecnicas_avanzadas_son_dificiles
  (intermedia(Tecnica), not(basica(Tecnica))). %Las_tecnicas_intermedias_que_no_son_basicas_tambien_son_dificiles

%Regla_para_verificar_el_nivel_de_habilidad_de_una_tecnica
nivel_habilidad(Tecnica, avanzada) :-
avanzada(tecnica(Tecnica)).
nivel_habilidad(Tecnica, intermedia) :-
intermedia(tecnica(Tecnica)).
nivel_habilidad(Tecnica, basica) :-
basica(tecnica(Tecnica)).

%Categorias_de_olas_segun_su_tamano
categoria_ola(tipo_ola(Tipo), lenta) :- 
    member(Tipo, [pequena, mediana, grande]).
categoria_ola(tipo_ola(Tipo), rapida) :- 
    member(Tipo, [pequena, mediana, grande]).
categoria_ola(tipo_ola(Tipo), limpia) :- 
    member(Tipo, [pequena, mediana, grande]).
categoria_ola(tipo_ola(Tipo), choppy) :- 
    member(Tipo, [pequena, mediana, grande]).

%Categorias_de_olas_segun_su_forma
categoria_ola(tipo_ola(barrel), hueca).
categoria_ola(tipo_ola(Tipo), corta) :- 
    member(Tipo, [fat, corta]).
categoria_ola(tipo_ola(Tipo), ondulada) :- 
    member(Tipo, [ondulante, espesa]).

%Categorias_de_olas_segun_su_ubicacion
categoria_ola(tipo_ola(Tipo), beach_break) :- 
    member(Tipo, [beach_break]).
categoria_ola(tipo_ola(Tipo), point_break) :- 
    member(Tipo, [point_break]).
categoria_ola(tipo_ola(Tipo), reef_break) :- 
    member(Tipo, [reef_break]).

%Categorias_de_olas_segun_su_comportamiento
categoria_ola(tipo_ola(Tipo), marea_vacilante) :- 
    member(Tipo, [marea_vacilante]).
categoria_ola(tipo_ola(Tipo), cerrada) :- 
    member(Tipo, [cerrada]).
categoria_ola(tipo_ola(Tipo), abierta) :- 
    member(Tipo, [abierta]).

%Regla_para_obtener_la_categoria_de_una_caracteristica_de_playa
categoria_caracteristica_playa(Caracteristica, Categoria) :-
categoria_playa(Caracteristica, Categoria).

%Terminologia_del_surf

termino_tecnico(Termino) :-
member(Termino, [tube, wipeout, duck_dive, cutback, surfista_goofy,
surfista_regular, inside, outside, air, barrel,
bottom_turn, drop_in, face, floater, kook, lip,
nose, off_the_lip, snap, tail, wax]).

%Temperatura_del_agua_en_grados_Celsius_y_condiciones_de_surf
condiciones_apropiadas(Playa) :-
temperatura_agua(Playa, Temp),
(Temp >= 20; (Temp >= 18, condiciones(Playa, nublado)); (Temp >= 16, condiciones(Playa, ventoso)); condiciones(Playa, tormenta)).

%Tamano_maximo_de_olas_por_nivel_de_habilidad_y_duracion

condiciones_apropiadas_por_habilidad(Playa, Habilidad) :-
tamanio_maximo(Habilidad, Tam),
duracion_ola(Tam, Dur),
duracion_olas_apropiadas(Playa, Dur).

duracion_olas_apropiadas(Playa, Dur) :-
temperatura_agua(Playa, Temp),
(Temp >= 20; (Temp >= 18, Dur < 10); (Temp >= 16, Dur < 15); Dur < 20),
(condiciones(Playa, soleado); condiciones(Playa, nublado)).


%Regla_para_obtener_la_duracion_de_una_ola
duracion_nivel_habilidad(NivelHabilidad, Duracion) :-
    tamanio_maximo(NivelHabilidad, Tamano),
    duracion_ola(Tamano, Duracion).

%Regla_para_obtener_la_temperatura_del_agua 
temperatura_playa(Playa, Temperatura) :-
    temperatura_agua(Playa, Temperatura).

%Regla_para_obtener_las_condiciones_de_surf_para_una_playa_dada
condiciones_playa(Playa, Condiciones) :-
    condiciones(Playa, Condiciones).

%Reglapara_obtener_los_beneficios_fisicos_del_surf
beneficios_fisicos(Beneficios) :-
    findall(B, beneficio_fisico(B), Beneficios).


%reglapara_obtene_la_sede_y_la_fecha_de_una_competencia_d_surf_dada
sede_fecha(Competencia, Sede, Fecha) :-
    sede(Competencia, Sede),
    fecha(Competencia, Fecha).

%Regla_para_obtener_el_ganador_de_una_competencia 
ganador_competencia(Competencia, Ganador) :-
    ganador(Competencia, Ganador).


%reglaa_para_condiciones
condiciones_actuales(Lugar, Temperatura, Condiciones) :-
    temperatura_agua(Lugar, Temperatura),
    condiciones(Lugar, Condiciones).


%Reglas_sobre_puntajes
puntaje_calificacion(Calificacion, Puntaje) :- 
    puntaje(Calificacion, Puntaje).
calificacion_aceptable(Calificacion) :-
    puntaje(Calificacion, Puntaje),
    Puntaje > 0.
calificacion_puntaje(Puntaje, Calificacion) :-
    puntaje(Calificacion, Puntaje).
puntaje_valido(Puntaje) :-
    puntaje(_, Puntaje).

%Reglas_sobre_jueces 

total_jueces(Total) :-
    findall(_, juez(_), Jueces),
    length(Jueces, Total).

juez_existente(Juez) :-
    juez(Juez).

lista_jueces(Jueces) :-
    findall(Juez, juez(Juez), Jueces).

jueces_diferentes(Juez1, Juez2) :-
    juez(Juez1),
    juez(Juez2),
    Juez1 \= Juez2.
lista_jueces_excepto(Juez, Jueces) :-
    findall(OtroJuez, (juez(OtroJuez), OtroJuez \= Juez), Jueces).
