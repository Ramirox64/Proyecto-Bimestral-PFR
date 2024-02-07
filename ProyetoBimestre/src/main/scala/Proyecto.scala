import com.github.tototoshi.csv.*
import org.nspl.awtrenderer.*
import org.nspl.*
import org.nspl.data.HistogramData
import org.nspl.data.HistogramData.*
import org.saddle.{Index, Series, Vec}

import java.io.File

implicit object CustomFormat extends DefaultCSVFormat {
  override val delimiter: Char = ';'
}

object Proyecto {
  @main
  def work() = {

    val path2DataFile: String = "C:\\Users\\USUARIO WIN10\\Desktop\\Pintegrador\\dsAlineacionesXTorneo.csv"
    val reader = CSVReader.open(new File(path2DataFile))
    val contenFile: List[Map[String, String]] = reader.allWithHeaders()

    reader.close

    val pathDataFile2: String = "C:\\Users\\USUARIO WIN10\\Desktop\\Pintegrador\\dsPartidosYGoles.csv"
    val reader2 = CSVReader.open(new File(pathDataFile2))
    val contentFile2: List[Map[String, String]] = reader2.allWithHeaders()

    reader2.close()

    chartingGoalPerMinute(contentFile2)
    charting(contenFile)
    chartingWinnerTournament(contentFile2)

    charStageGoals(contentFile2)

    chartTotalGoalsTourMen(contentFile2)
    chartTotalGoalsTourWomen(contentFile2)
    chartStadiumGoals(contentFile2)


  }

  //Goles totales de mujeres en mundiales
  def chartTotalGoalsTourWomen(data: List[Map[String, String]]): Unit = {
    val data4Chart = data
      .map(x => (x("matches_tournament_id"), x("goals_goal_id"), x("tournaments_tournament_name")))
      .distinct
      .filter(x => x._3.contains("FIFA Women's World Cup"))
      .groupBy(x => x._1)
      .map(x => x._1.split("-")(1).toDouble -> x._2.length.toDouble)
      .toIndexedSeq


    val plot=xyplot(data4Chart)(
      par
        .withXLab("Mundial")
        .ylab("Goles")
        .xLabelRotation(-77)
        .xNumTicks(0)
        .main("Cantidad de goles de Mujeres")
    )
    pngToFile(new File("C:\\Users\\USUARIO WIN10\\Desktop\\Pintegrador\\TotalGoalTourWomen.png"), plot.build, 400)

  }

  //Goles Totales en cada estadio
  def chartStadiumGoals(data: List[Map[String, String]]): Unit = {
    val data4Chart = data
      .map(x => (x("stadiums_city_name"), x("goals_goal_id")))
      .distinct
      .groupBy(x => x._1)
      .map(x => x._1 -> x._2.length.toDouble)
    val indices = Index(data4Chart.map(value => value._1).toArray)
    val values = Vec(data4Chart.map(value => value._2).toArray)

    val series = Series(indices, values)

    val barStadiumGoal = saddle.barplotHorizontal(series,
      xLabFontSize = Option(RelFontSize(0.1)),
      color = GrayScale(10, 200))(
      par
        .xlab("Estadios")
        .ylab("Goles")
        .xLabelRotation(-77)
        .xNumTicks(0)
        .main("Cantidad de goles por Estadio"))
    pngToFile(new File("C:\\Users\\USUARIO WIN10\\Desktop\\Pintegrador\\StadiumGoal.png"), barStadiumGoal.build, 400)

  }

  //goles totales de hombres en mundiales
  def chartTotalGoalsTourMen(data: List[Map[String, String]]): Unit = {
    val data4Chart = data
      .map(x => (x("matches_tournament_id"), x("goals_goal_id"), x("tournaments_tournament_name")))
      .distinct
      .filter(x => x._3.contains("FIFA Men's World Cup"))
      .groupBy(x => x._1)
      .map(x => x._1 -> x._2.length.toDouble)
    val indices = Index(data4Chart.map(value => value._1).toArray)
    val values = Vec(data4Chart.map(value => value._2).toArray)

    val series = Series(indices, values)

    val bar1 = saddle.barplotHorizontal(series,
      xLabFontSize = Option(RelFontSize(0.6)),
      color = RedBlue(0, 5))(
      par
        .xlab("Mundial")
        .ylab("Goles")
        .xLabelRotation(-77)
        .xNumTicks(0)
        .main("Goles Totales Por Mundial Masculino"))
    pngToFile(new File("C:\\Users\\USUARIO WIN10\\Desktop\\Pintegrador\\TotalGoalTourMen.png"), bar1.build, 400)

  }

  //Goles totales de cada fase del mundial
  def charStageGoals(data: List[Map[String, String]]): Unit = {
    val data4Chart = data
      .map(x => (x("matches_stage_name"), x("goals_goal_id")))
      .distinct
      .groupBy(x => x._1)
      .map(x => x._1 -> x._2.length.toDouble)

    val indices = Index(data4Chart.map(value => value._1).toArray)
    val values = Vec(data4Chart.map(value => value._2).toArray)

    val series = Series(indices, values)

    val barStageGoal = saddle.barplotHorizontal(series,
      xLabFontSize = Option(RelFontSize(1)),
      color = RedBlue(0, 5))(
      par
        .xlab("Fase")
        .ylab("Goles")
        .xLabelRotation(-77)
        .xNumTicks(0)
        .main("Canidad de Goles por Fase del Mundial"))
    pngToFile(new File("C:\\Users\\USUARIO WIN10\\Desktop\\Pintegrador\\StagesGoals.png"), barStageGoal.build, 400)

  }

  // Número de veces que han campeonado los países
  def chartingWinnerTournament(data: List[Map[String, String]]): Unit = {
    val data4Chart = data
      .map(x => (x("tournaments_winner"), x("tournaments_year")))
      .distinct
      .groupBy(x => x._1)
      .map(x => x._1 -> x._2.length.toDouble)

    val indices = Index(data4Chart.map(value => value._1).toArray)
    val values = Vec(data4Chart.map(value => value._2).toArray)

    val series = Series(indices, values)

    val barTourWin = saddle.barplotHorizontal(series,
      xLabFontSize = Option(RelFontSize(1)),
      color = RedBlue(0, 5))(
      par
        .xlab("Campeones")
        .ylab("Nmro de Campeonatos")
        .xLabelRotation(-77)
        .xNumTicks(0)
        .main("Número de campeonatos Mundiales"))
    pngToFile(new File("C:\\Users\\USUARIO WIN10\\Desktop\\Pintegrador\\WinnerTour.png"), barTourWin.build, 400)
  }

  //Ratio de goles por minuto
  def chartingGoalPerMinute(data: List[Map[String, String]]): Unit = {
    val data4Chart = data
      .filter(x => x("goals_minute_regulation") != "NA")
      .map(x => (x("goals_minute_regulation").toDouble, x("goals_goal_id")))
      .map(x => x._1 -> x._2) //
      .groupBy(_._1)
      .map(x => (x._1.toString, x._2.length.toDouble))

    val indices = Index(data4Chart.map(value => value._1).toArray)
    val values = Vec(data4Chart.map(value => value._2).toArray)

    val series = Series(indices, values)

    val barGoalPerMinute = saddle.barplotHorizontal(series,
      xLabFontSize = Option(RelFontSize(0.2)),
      color = RedBlue(86, 186))(
      par
        .xlab("Minuto")
        .ylab("freq.")
        .xLabelRotation(-77)
        .xNumTicks(0)
        .main("Goles por Minuto"))
    pngToFile(new File("C:\\Users\\USUARIO WIN10\\Desktop\\Pintegrador\\GoalPerMinute.png"), barGoalPerMinute.build, 400)
  }


  def charting(data: List[Map[String, String]]): Unit = {
    val listNroShirt: List[Double] = data
      .filter(row => row("squads_position_name") == "forward" && row("squads_shirt_number") != "0")
      .map(row => row("squads_shirt_number").toDouble)

    val histForwardShirtNumber = xyplot(HistogramData(listNroShirt, 10) -> bar())(
      par
        .xlab("shirt number")
        .ylab("freq.")
        .main("Forward Shirt Number")
    )
    pngToFile(new File("C:\\Users\\USUARIO WIN10\\Desktop\\Pintegrador\\fhsn.png"), histForwardShirtNumber.build, 1000)

  }
}