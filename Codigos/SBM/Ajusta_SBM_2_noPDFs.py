# Codigo para obtener el mejor ajuste de SBM usando graph-tool.

# Importamos librerias
import matplotlib
matplotlib.use('PDF')
import pandas as pd
import numpy as np
import graph_tool.all as gt
import os
import matplotlib.pyplot as plt
from pylab import *
import copy
import collections
from tqdm import tqdm

# Solicitamos las rutas de trabajo

# Ruta donde se va a escribir la salida
rutaPrin = input("Escriba la ruta donde se escribira la salida:\n")
# Cambiamos la ruta
os.chdir(rutaPrin)

# Ruta donde se encuentran los objetos de red
rutaObj = input("Escriba la ruta donde estan los objetos de red:\n")
# Extraemos la lista con los nombres de los archivos
archivos = os.listdir(rutaObj)
archivos = np.array(archivos)
archivos = np.sort(archivos)

# Solicitamos un numero de simulaciones para los SBM
sims = input("Escriba el número de simulaciones para SBM")
sims = int(sims)

# En un for recorreremo cada uno de los objetos y procesaremos
for ar in tqdm(archivos):
    # Cargamos la red
    red = gt.load_graph(rutaObj + ar)

    # Extraemos la fecha
    fecha = ar[0:10]

    # Creamos una copia de la red para quitarle los nodos
    # desconectados
    red_sinDesc = copy.deepcopy(red)
    vertices = red_sinDesc.vertices()

    # Recorremos los nodos y guardamos los que tengan grado total
    # igual a cero
    vert_desc = list()
    for v in vertices:
        tuple_grado = (v.out_degree(), v.in_degree())
        grado_out, grado_in = tuple_grado

        if grado_out == grado_in and grado_out == 0:
            vert_desc.append(v)


    # Quitamos los desconectados
    red_sinDesc.remove_vertex(vert_desc)

    # Ahora, realizamos el analisis de la distribucion
    # de grado (solo visualizacion)
    outDegree_FULL = np.array(red.get_out_degrees(red.get_vertices()))
    inDegree_FULL = np.array(red.get_in_degrees(red.get_vertices()))

    # Obtenemos el grado total
    grado_FULL = outDegree_FULL + inDegree_FULL

 
    # Obtenemos el conteo para el histograma del grado
    # (distribucion del grado).
    grado_FULL = np.sort(grado_FULL)
    gradoCount_FULL = collections.Counter(grado_FULL)
    grado, conteo = zip(*gradoCount_FULL.items())

    # Graficamos y guardamos
    tituloGraf = "DisGrado_" + fecha + ".pdf"
    plt.scatter(x=grado, y=conteo)
    plt.title("Distribucion del grado")
    plt.xlabel("$N_{k}$")
    plt.ylabel("Frecuencia")
    plt.tight_layout()
    savefig(tituloGraf, bbox_inches='tight')
    plt.close()

    # Ahora con escala log-log
    tituloGraf = "DisGrado_log-log_" + fecha + ".pdf"
    plt.scatter(x=grado, y=conteo)
    plt.xscale('log')
    plt.yscale('log')
    plt.title("Distribucion del grado (log-log)")
    plt.xlabel("$log(N_{k})$")
    plt.ylabel("log(Frecuencia)")
    plt.tight_layout()
    savefig(tituloGraf, bbox_inches='tight')
    plt.close()

    # Comparamos la distribucion con la de una red aleatoria
    # con el mismo grado promedio (poisson)
    gradoProm_FULL = np.mean(grado_FULL)
    poisson_FULL = np.random.poisson(lam=gradoProm_FULL, size=len(grado_FULL))
    poissonCount_FULL = collections.Counter(poisson_FULL)
    gradoPois, conteoPois = zip(*poissonCount_FULL.items())

    # Graficamos
    tituloGraf = "DistGradovsAlea_" + fecha + ".pdf"
    plt.scatter(x=grado, y=conteo)
    plt.scatter(x=gradoPois, y=conteoPois)
    plt.legend(["Observada", "Aleatoria"], bbox_to_anchor=(1.05, 1), loc=2)
    plt.title("Distribucion observada vs aleatoria")
    plt.xlabel("$N_{k}$")
    plt.ylabel("Frecuencia")
    plt.tight_layout()
    savefig(tituloGraf, bbox_inches='tight')
    plt.close()

    # Con escala log-log
    tituloGraf = "DistGradovsAlea_log-log_" + fecha + ".pdf"
    plt.scatter(x=grado, y=conteo)
    plt.scatter(x=gradoPois, y=conteoPois)
    plt.legend(["Observada", "Aleatoria"], bbox_to_anchor=(1.05, 1), loc=2)
    plt.xscale("log")
    plt.yscale("log")
    plt.title("Distribucion observada vs aleatoria (log-log)")
    plt.xlabel("$log(N_{k})$")
    plt.ylabel("log(Frecuencia)")
    plt.tight_layout()
    savefig(tituloGraf, bbox_inches='tight')
    plt.close()

    # Ahora, repetimos el analisis con la red sin nodos desconectados.
    outDegree_SinDesc = np.array(red_sinDesc.get_out_degrees(red_sinDesc.get_vertices()))
    inDegree_SinDesc = np.array(red_sinDesc.get_out_degrees(red_sinDesc.get_vertices()))
    grado_SinDesc = outDegree_SinDesc + inDegree_SinDesc

    # Obtenemos datos para el grafico de disperion
    grado_SinDesc = np.sort(grado_SinDesc)
    gradoCount_SinDesc = collections.Counter(grado_SinDesc)
    grado, conteo = zip(*gradoCount_SinDesc.items())

    # Graficamos
    tituloGraf = "DistGradoSinDesc_" + fecha + ".pdf"
    plt.scatter(x=grado, y=conteo)
    plt.title("Distribucion del grado")
    plt.xlabel("$N_{k}$")
    plt.ylabel("Frecuencia")
    plt.tight_layout()
    savefig(tituloGraf, bbox_inches='tight')
    plt.close()

    # Con escala log-log
    tituloGraf = "DisGradoSinDesc_log-log_" + fecha + ".pdf"
    plt.scatter(x=grado, y=conteo)
    plt.xscale('log')
    plt.yscale('log')
    plt.title("Distribucion del grado (log-log)")
    plt.xlabel("$log(N_{k})$")
    plt.ylabel("log(Frecuencia)")
    plt.tight_layout()
    savefig(tituloGraf, bbox_inches='tight')
    plt.close()

    # Ahora, agregamos la distribucion aleatoria
    gradoProm_SinDesc = np.mean(grado_SinDesc)

    # Simulamos
    poisson_SinDesc = np.random.poisson(lam=gradoProm_SinDesc,
                                        size=len(grado_SinDesc))
    poissonCount_SinDesc = collections.Counter(poisson_SinDesc)
    gradoPois, conteoPois = zip(*poissonCount_SinDesc.items())

    # Graficamos
    tituloGraf = "DistGradovsAleaSinDesc_" + fecha + ".pdf"
    plt.scatter(x=grado, y=conteo)
    plt.scatter(x=gradoPois, y=conteoPois)
    plt.legend(["Observada", "Aleatoria"], bbox_to_anchor=(1.05, 1), loc=2)
    plt.title("Distribucion observada vs aleatoria")
    plt.xlabel("$N_{k}$")
    plt.ylabel("Frecuencia")
    plt.tight_layout()
    savefig(tituloGraf, bbox_inches='tight')
    plt.close()

    # COn escala log-log
    tituloGraf = "DistGradovsAleaSinDesc_log-log_" + fecha + ".pdf"
    plt.scatter(x=grado, y=conteo)
    plt.scatter(x=gradoPois, y=conteoPois)
    plt.legend(["Observada", "Aleatoria"], bbox_to_anchor=(1.05, 1), loc=2)
    plt.xscale("log")
    plt.yscale("log")
    plt.title("Distribucion observada vs aleatoria (log-log)")
    plt.xlabel("$log(N_{k})$")
    plt.ylabel("log(Frecuencia)")
    plt.tight_layout()
    savefig(tituloGraf, bbox_inches='tight')
    plt.close()

    # ============ Visualizacion de las redes ==============
    pesos = red.edge_properties['weight']
    pesosSinDesc = red_sinDesc.edge_properties["weight"]

    # Calculamos algunas medidas para darle formato a las graficas
    pagerank = gt.pagerank(red)
    pagerankSinDesc = gt.pagerank(red_sinDesc)
    betweenness = gt.betweenness(red)
    betweennessSinDesc = gt.betweenness(red_sinDesc)



    # =========== Ajuste de los Bloques Estocasticos ============

    # Primero, obtenemos los ajustes sin correccion de grado
    ajustes_NDC = [gt.minimize_blockmodel_dl(red, state_args=dict(deg_corr=False)) for i in range(sims)]
    ajustesSinDesc_NDC = [gt.minimize_blockmodel_dl(red_sinDesc, state_args=dict(deg_corr=False)) for i in range(sims)]

    # Obtenemos el vector con la description lenght de cada ajuste para
    # seleccionar el mejor
    mdl_NDC = list()
    for i in range(len(ajustes_NDC)):
        mdl_NDC.append(ajustes_NDC[i].entropy())

    # Obtenemos el indice del mejor ajuste
    ind_bestNDC = mdl_NDC.index(min(mdl_NDC))

    # Lo seleccionamos
    best_NDC = ajustes_NDC[ind_bestNDC]

    # Repetimos con los ajustes sin nodos desconectados
    mdlSinDesc_NDC = list()
    for i in range(len(ajustesSinDesc_NDC)):
        mdlSinDesc_NDC.append(ajustesSinDesc_NDC[i].entropy())

    ind_bestSinDescNDC = mdlSinDesc_NDC.index(min(mdlSinDesc_NDC))

    bestSinDesc_NDC = ajustesSinDesc_NDC[ind_bestSinDescNDC]
        

    # Ahora, hacemos los ajustes con correccion de grado
    ajustes_DC = [gt.minimize_blockmodel_dl(red, state_args=dict(deg_corr=True)) for i in range(sims)]
    ajustesSinDesc_DC = [gt.minimize_blockmodel_dl(red_sinDesc, state_args=dict(deg_corr=True)) for i in range(sims)]

    # Obtenemos el mejor ajuste
    mdl_DC = list()
    for i in range(len(ajustes_DC)):
        mdl_DC.append(ajustes_DC[i].entropy())

    # El indice en el arreglo
    ind_bestDC = mdl_DC.index(min(mdl_DC))

    # Seleccionamos
    best_DC = ajustes_DC[ind_bestDC]

    # Repetimos con el ajuste sin nodos desconectados
    mdlSinDesc_DC = list()
    for i in range(len(ajustesSinDesc_DC)):
        mdlSinDesc_DC.append(ajustesSinDesc_DC[i].entropy())

    ind_bestSinDescDC = mdlSinDesc_DC.index(min(mdlSinDesc_DC))

    bestSinDesc_DC = ajustesSinDesc_DC[ind_bestSinDescDC]


    # ============== Obtención de las medidas de red por grupo ========

    # Obtenemos los indices de los nodos
    indFULL = np.array(red.vertex_index.copy().get_array())
    indSInDesc = np.array(red_sinDesc.vertex_index.copy().get_array())

    # Obtenemos algunas medidas más de centralidad
    # PageRank
    pr = gt.pagerank(red, weight=pesos)
    # Betweenness
    bet = gt.betweenness(red, weight=pesos)
    # Closeness
    close = gt.closeness(red, weight=pesos)
    # Eigenvector centrality
    eigen = gt.eigenvector(red, weight=pesos)
    # Katz
    katz = gt.katz(red, weight=pesos)
    # Hits
    hits = gt.hits(red, weight=pesos)

    # Para red sin desconectados
    prSinDesc = gt.pagerank(red_sinDesc, weight=pesosSinDesc)
    betSinDesc = gt.betweenness(red_sinDesc, weight=pesosSinDesc)
    closSinDesc = gt.closeness(red_sinDesc, weight=pesosSinDesc)
    eigenSinDesc = gt.eigenvector(red_sinDesc, weight=pesosSinDesc)
    katzSinDesc = gt.katz(red_sinDesc, weight=pesosSinDesc)
    hitsSinDesc = gt.hits(red_sinDesc, weight=pesosSinDesc)

    # Obtenemos el nombre de las instituciones
    institucionesFULL = list()
    for i in range(len(red.get_vertices())):
        institucionesFULL.append(red.vertex_properties['name'][i])

    # SIn desconectados
    institucionesSinDesc = list()
    for i in range(len(red_sinDesc.get_vertices())):
        institucionesSinDesc.append(red_sinDesc.vertex_properties['name'][i])

    # Es necesario agregar la fecha a los datos
    fechasFULL = np.array(repeat(fecha, repeats=len(institucionesFULL)))
    fechasSinDesc = np.array(repeat(fecha, repeats=len(institucionesSinDesc)))

    # Pegamos todo en un solo data frame
    medidasFULL = np.column_stack((fechasFULL,
                                   indFULL,
                                   pr.get_array(),
                                   bet[0].get_array(),
                                   close.get_array(),
                                   eigen[1].get_array(),
                                   katz.get_array(),
                                   hits[1].get_array(),
                                   best_NDC.get_blocks().get_array(),
                                   best_DC.get_blocks().get_array()))
    # Agregamos la institucion
    medidasFULL_DF = pd.DataFrame(medidasFULL, columns=["Fecha",
                                                        "Inst",
                                                        "PageRank",
                                                        "Betweenness",
                                                        "Closeness",
                                                        "Eigenvector",
                                                        "Katz",
                                                        "Hits",
                                                        "Grupo_NDC",
                                                        "Grupo_DC"],
                                  index=institucionesFULL)
    # Escribimos el resultado
    nombreFile = "Medidas_FULL_" + fecha + ".csv"
    medidasFULL_DF.to_csv(nombreFile, sep=",", index=True)

    # Ahora, con la red sin nodos desconectados
    medidas_SinDesc = np.column_stack((fechasSinDesc,
                                       indSInDesc,
                                       prSinDesc.get_array(),
                                       betSinDesc[0].get_array(),
                                       closSinDesc.get_array(),
                                       eigenSinDesc[1].get_array(),
                                       katzSinDesc.get_array(),
                                       hitsSinDesc[1].get_array(),
                                       bestSinDesc_NDC.get_blocks().get_array(),
                                       bestSinDesc_DC.get_blocks().get_array()))
    # Hacemos data frame
    medidas_SinDesc_DF = pd.DataFrame(medidas_SinDesc, columns=["Fecha",
                                                                "Inst",
                                                                "PageRank",
                                                                "Betweenness",
                                                                "Closeness",
                                                                "Eigenvector",
                                                                "Katz",
                                                                "Hits",
                                                                "Grupo_NDC",
                                                                "Grupo_DC"],
                                      index=institucionesSinDesc)

    # Escribimos a csv
    nombreFile = "Medidas_SinDesc_" + fecha + ".csv"
    medidas_SinDesc_DF.to_csv(nombreFile, sep=",", index=True)

    # Por ultimo, creamos un data frame con el grado de la red, para hacer
    # el analisis de la distribucion del grado.
    grado = np.column_stack((fechasFULL,
                             outDegree_FULL,
                             inDegree_FULL,
                             grado_FULL))

    # Creamos data frame
    grado_DF = pd.DataFrame(grado, columns=["Fecha",
                                            "Grado_Out",
                                            "Grado_In",
                                            "Grado_Tot"],
                            index =institucionesFULL)

    # Escribimos
    nombreFile = "Grado_FULL_" + fecha + ".csv"
    grado_DF.to_csv(nombreFile, sep=",", index=True)
