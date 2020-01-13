/**
 * @file ProcessProportionsAtAge.cpp
 * @author Scott Rasmussen (scott.rasmussen@zaita.com)
 * @github https://github.com/Zaita
 * @date 17/10/2014
 * @section LICENSE
 *
 * Copyright NIWA Science �2014 - www.niwa.co.nz
 *
 */

// headers
#include "Process.h"

#include <memory>

#include "TimeSteps/Manager.h"
#include "Utilities/DoubleCompare.h"

// namespaces
namespace niwa {
namespace observations {
namespace age {

/**
 *
 */
ProcessProportionsAtAge::ProcessProportionsAtAge(Model* model)
   : observations::age::ProportionsAtAge(model) {

  parameters_.Bind<string>(PARAM_PROCESS, &process_label_, "The label of the process for the observation", "");
  parameters_.Bind<double>(PARAM_PROCESS_PROPORTION, &process_proportion_, "Proportion through the process when the observation is evaluated", "", double(0.5))->set_range(0.0, 1.0);

  mean_proportion_method_ = false;
}

/**
 *
 */
void ProcessProportionsAtAge::DoBuild() {
  ProportionsAtAge::DoBuild();

  if (process_proportion_ < 0.0 || process_proportion_ > 1.0)
    LOG_ERROR_P(PARAM_PROCESS_PROPORTION) << ": process_proportion (" << process_proportion_ << ") must be between 0.0 and 1.0";
  proportion_of_time_ = process_proportion_;

  auto time_step = model_->managers().time_step()->GetTimeStep(time_step_label_);
  if (!time_step) {
    LOG_FATAL_P(PARAM_TIME_STEP) << time_step_label_ << " was not found. Has it been defined?";
  } else {
    auto process = time_step->SubscribeToProcess(this, years_, process_label_);
    mortality_instantaneous_ = dynamic_cast<processes::age::MortalityInstantaneous*>(process);
  }
}

} /* namespace age */
} /* namespace observations */
} /* namespace niwa */
