package models

import org.joda.time.DateTime
import scala.xml.XML
import java.math.BigDecimal

case class Usage (custID:Int) {

    val xmlBaseDir = "/home/ec2-user/savemoney/public/"

    def parseXML (custID:Int) = {
        val fileName = custID.toString + ".XML"
        val xmlFile = xmlBaseDir + fileName

        val intervalData = scala.xml.XML.loadFile(xmlFile) \\ "IntervalBlock"
        intervalData
    }

    val intervalData = parseXML(custID)

    val data = intervalData.view.map(_ \\ "value").map(_.map(_.text.toInt)).toList

    val t = intervalData.view.map(_ \\ "start").map(_.map(_.text.toInt)).toList

    val dates = t.map (x => new DateTime(x.head.toLong * 1000))
    val month = dates.map (x => x.getMonthOfYear)
    val monthid = dates.map (x => x.getMonthOfYear + (x.getYear * 100) )
    val season = month.map (x => x match {
        case x if 5 to 10 contains x => 0
        case _ => 1
      }
    )


    /* Get list of months in current dataset */

    val months = monthid.distinct


    /* Get index for all meter reads for a month */

    def getIndexForMonthID(monthID:Int) = {
        monthid.zipWithIndex.filter(_._1 == monthID).map(_._2)
    }



    /* Tariff Configuration */

    val hours = List(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)
    val hoursOffPeak = List(5,6,7,8,9,10,11,18,19,20,21,22,23)
    val hoursOnPeak = List(12,13,14,16,17)
    val hoursSuperOffPeak = List(0,1,2,3,4)

    val priceOffPeak = 19
    val priceOnPeak = 29
    val priceSuperOffPeak = 16

    val pricesByTierSummer = List(0,15,17,35,37)
    val pricesByTierWinter = List(0,15,17,33,35)
    val tierThresholdsPct = List(0,100,130,200)


    /* Baselines by Region */

    val baselineCoastal = (294,498)
    val baselineInland = (330,549)
    val baselineMountain = (519,855)
    val baselineDesert = (585,660)

    /* prices in cents per kWh */






    /* Miscellaneous Usage Stats */

    val usageByDay = data.map(_.sum)
    val usageByHour = hours.map(num => data.map(_(num)))

    val avgUsagePerDay = usageByDay.sum / usageByDay.length
    val avgUsageByHour = usageByHour.map( usage => usage.sum / usage.length )

    val avgUsageOffPeak = hoursOffPeak.map(num => avgUsageByHour(num))
    val avgUsageOnPeak = hoursOnPeak.map(num => avgUsageByHour(num))
    val avgUsageSuperOffPeak = hoursSuperOffPeak.map(num => avgUsageByHour(num))




    def getMonthData(monthID:Int) = {
      val monthIDs = getIndexForMonthID(monthID)
      val monthData = monthIDs.map(id => data(id).toList)
      monthData
    }


    def getBillEVTOU2(hourlyUsage: List[Int]) = {
        val usageOffPeak = hoursOffPeak.map(hour => hourlyUsage(hour))
        val usageOnPeak = hoursOnPeak.map(hour => hourlyUsage(hour))
        val usageSuperOffPeak = hoursSuperOffPeak.map(hour => hourlyUsage(hour))

        val amtOffPeak = usageOffPeak.sum * priceOffPeak / 100
        val amtOnPeak = usageOnPeak.sum * priceOnPeak / 100
        val amtSuperOffPeak = usageSuperOffPeak.sum * priceSuperOffPeak / 100
        val total = amtOffPeak + amtOnPeak + amtSuperOffPeak
        List(total, amtOffPeak, amtOnPeak, amtSuperOffPeak)
    }



    def getDailyAmtsEVTOU2(monthID: Int) = {
        val monthData = getMonthData(monthID)
        val dailyAmts = monthData.map(day => getBillEVTOU2(day))
        dailyAmts
    }

    def getMonthlyTotalEVTOU2(monthID: Int) = {
        val monthTotal = getDailyAmtsEVTOU2(monthID).map(day => day(0)).sum
        val monthTotalOffPeak = getDailyAmtsEVTOU2(monthID).map(day => day(1)).sum
        val monthTotalOnPeak = getDailyAmtsEVTOU2(monthID).map(day => day(2)).sum
        val monthTotalSuperOffPeak = getDailyAmtsEVTOU2(monthID).map(day => day(3)).sum
        List(monthTotal,monthTotalOffPeak,monthTotalOnPeak,monthTotalSuperOffPeak)
    }


    /* Get TOU Bill totals for each available month */

    def getEVTOU2Bills () = {
        months.map(monthID => 
            getMonthlyTotalEVTOU2(monthID)
        )
    }
    val bills = getEVTOU2Bills
    val billTotals = bills.map(x => x(0))
    val avgBillAmt = billTotals.sum / billTotals.length
    val totalAmt = billTotals.sum

}