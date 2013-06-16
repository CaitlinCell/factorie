package cc.factorie.optimize

import cc.factorie.{WeightsMap, WeightsSet}

/**
 * Simple efficient l2-regularized SGD with a constant learning rate
 *
 * Note that the learning rate has to be less than l2 or the weights will oscillate.
 *
 * @param l2 The l2 regularization parameter
 * @param rate The learning rate
 */
class L2RegularizedConstantRate(l2: Double = 0.1, rate: Double = 0.1) extends GradientOptimizer {
  private var initialized = false

  def step(weights: WeightsSet, gradient: WeightsMap, value: Double): Unit = {
    if (!initialized) { initializeWeights(weights); initialized = true }
    weights += (gradient, rate)
    weights *= (1.0 - rate * l2)
  }

  def initializeWeights(weights: WeightsSet) = MutableScalableWeights.initializeWeights(weights)
  def finalizeWeights(weights: WeightsSet) = MutableScalableWeights.finalizeWeights(weights)

  def isConverged = false
  def reset(): Unit = { }
}