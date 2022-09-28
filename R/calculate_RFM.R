#' Calculate RFM
#'
# Description
#' Calculate the weighted RFM score : recency, frequency and monetary for every customer
#'
#' # Arguments
#' @param data : A data table containing the transaction recorded for every customer
#' @param	weight_recency :   Weight of recency = a number
#' @param	weight_frequency : Weight of frequency = a number
#' @param	weight_monetary : Weight of monetary = a number
#'
#'@details
#'\code{data} contains the transactional data. The dataset must contain a
#'             column labeled "Customer" that allows unique customer identification
#'             and a column labeled "TransDate", indicating the purchase date.
#'             The column "PurchAmount" specifies the total spending per purchase.
#'
# Returned values
#' @return a  data.data containing the recency, frequency and monetary
#' score as well as the weighted final score and the group membership.
#'
#' @export



RFMfunction <- function (data, weight_recency=1, weight_frequency=1, weight_monetary=1){

  #adjusting values to ensure that the weights add up to one
  weight_recency2 <- weight_recency/sum(weight_recency, weight_frequency, weight_monetary)
  weight_frequency2 <- weight_frequency/sum(weight_recency, weight_frequency, weight_monetary)
  weight_monetary2 <- weight_monetary/sum(weight_recency, weight_frequency, weight_monetary)

  print("weights are calculated")

  data = data[, TransDate:=dmy(TransDate)]
  max.Date = max(data$TransDate)

  data = data[, list(recency=as.numeric(max.Date - max(TransDate)),
                     frequency = max(1:.N) ,
                     monetary = mean(PurchAmount)), by=Customer]

  print("RFM Measure done")

  #Create the RFM scores
  recency_score = as.numeric(cut2(-data$recency, g=3))
  frequency_score = as.numeric(cut2(data$frequency, g=3))
  monetary_score = as.numeric(cut2(data$monetary, g=3))

  #Create the RFM table includid the RFM scores
  data = data[,list(Customer, recency_score , frequency_score , monetary_score)]

  #Overall RFM scores by their weight
  data[, final_score:=weight_recency2*recency_score+weight_frequency2*frequency_score+weight_monetary2*monetary_score]

  print("Overall RFM Measure done")

  # RFM group
  data[,group:=round(final_score)]

  # Return final table
  return(data)
}
