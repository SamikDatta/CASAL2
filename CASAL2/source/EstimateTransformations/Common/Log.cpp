/**
 * @file Log.cpp
 * @author Scott Rasmussen (scott.rasmussen@zaita.com)
 * @github https://github.com/Zaita
 * @date Jan 8, 2016
 * @section LICENSE
 *
 * Copyright NIWA Science �2016 - www.niwa.co.nz
 *
 */

// headers
#include "Log.h"

#include "Model/Model.h"
#include "Model/Objects.h"
#include "Model/Managers.h"
#include "Estimates/Manager.h"
#include "Estimates/Estimate.h"

// namespaces
namespace niwa {
namespace estimatetransformations {

/**
 * Default constructor
 */
Log::Log(Model* model) : EstimateTransformation(model) {
  parameters_.Bind<string>(PARAM_ESTIMATE_LABEL, &estimate_label_, "Label of estimate block to apply transformation. Defined as $\theta_1$ in the documentation", "");
}

/**
 */
void Log::DoValidate() {

}

/**
 *
 */
void Log::DoBuild() {
  LOG_FINEST() << "transformation on @estimate " << estimate_label_;
  estimate_ = model_->managers().estimate()->GetEstimateByLabel(estimate_label_);
  if (estimate_ == nullptr) {
    LOG_ERROR_P(PARAM_ESTIMATE) << "Estimate " << estimate_label_ << " could not be found. Have you defined it?";
    return;
  }
  // Initialise for -r runs
  current_untransformed_value_ = estimate_->value();

  LOG_FINE() << "transform with objective = " << transform_with_jacobian_ << " estimeate transform " << estimate_->transform_for_objective() << " together = " << !transform_with_jacobian_ && !estimate_->transform_for_objective();
  if (!transform_with_jacobian_ && !estimate_->transform_for_objective()) {
    LOG_ERROR_P(PARAM_TRANSFORM_WITH_JACOBIAN) << "You have specified a transformation that does not contribute a jacobian, and the prior parameters do not refer to the transformed estimate, in the @estimate" << estimate_label_ << ". This is not advised, and may cause bias estimation. Please address the user manual if you need help";
  }
  if (estimate_->transform_with_jacobian_is_defined()) {
    if (transform_with_jacobian_ != estimate_->transform_with_jacobian()) {
      LOG_ERROR_P(PARAM_TRANSFORM_WITH_JACOBIAN) << "This parameter is not consistent with the equivalent parameter in the @estimate block " << estimate_label_ << ". please make sure these are both true or both false.";
    }
  }

  if (!estimate_->transform_for_objective()) {
    estimate_->set_lower_bound(log(estimate_->lower_bound()));
    estimate_->set_upper_bound(log(estimate_->upper_bound()));
  }

  LOG_FINEST() << "Finish DoBuild()";

}

/**
 *
 */
void Log::DoTransform() {
  LOG_MEDIUM() << "parameter before transform = " << estimate_->value() << " lower bound " << lower_bound_ << " upper bound " << upper_bound_;
  current_untransformed_value_ = estimate_->value();
  estimate_->set_value(log(estimate_->value()));
  LOG_MEDIUM() << "parameter after transform = " << estimate_->value() << " lower bound " << estimate_->lower_bound() << " upper bound " << estimate_->upper_bound();
}

/**
 *
 */
void Log::DoRestore() {
    estimate_->set_value(exp(estimate_->value()));
}

/**
 * This method will check if the estimate needs to be transformed for the objective function. If it does then
 * it'll do the transformation.
 */
void Log::TransformForObjectiveFunction() {
  if (estimate_->transform_for_objective())
    Transform();
}

/**
 * This method will check if the estimate needs to be Restored from the objective function. If it does then
 * it'll do the undo the transformation.
 */
void Log::RestoreFromObjectiveFunction() {
  if (estimate_->transform_for_objective())
    Restore();
}


/**
 *
 */
Double Log::GetScore() {
  if(transform_with_jacobian_) {
    jacobian_ = 1.0 / current_untransformed_value_;
    LOG_MEDIUM() << "jacobian: " << jacobian_;
    return jacobian_;
  } else
    return 0.0;
}

/**
 * Get the target addressables so we can ensure each
 * object is not referencing multiple ones as this would
 * cause chain issues
 *
 * @return Set of addressable labels
 */
std::set<string> Log::GetTargetEstimates() {
  set<string> result;
  result.insert(estimate_label_);
  return result;
}
} /* namespace estimatetransformations */
} /* namespace niwa */
