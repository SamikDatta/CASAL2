/**
 * @file Iterative.cpp
 * @author  Scott Rasmussen (scott.rasmussen@zaita.com)
 * @date 2/09/2014
 * @section LICENSE
 *
 * Copyright NIWA Science �2014 - www.niwa.co.nz
 *
 */

// headers
#include "Iterative.h"

#include <algorithm>
#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string/trim_all.hpp>

#include "../../Categories/Categories.h"
#include "../../DerivedQuantities/Manager.h"
#include "../../Partition/Accessors/Categories.h"
#include "../../Processes/Age/RecruitmentBevertonHolt.h"
#include "../../Processes/Age/RecruitmentBevertonHoltWithDeviations.h"
#include "../../Processes/Manager.h"
#include "../../TimeSteps/Factory.h"
#include "../../TimeSteps/Manager.h"

// namespaces
namespace niwa {
namespace initialisationphases {
namespace age {

namespace cached   = partition::accessors::cached;
namespace accessor = partition::accessors;

/**
 * Default constructor
 */
Iterative::Iterative(shared_ptr<Model> model) : InitialisationPhase(model), cached_partition_(model), partition_(model) {
  parameters_.Bind<unsigned>(PARAM_YEARS, &years_, "The number of iterations (years) over which to execute this initialisation phase", "");
  parameters_.Bind<string>(PARAM_INSERT_PROCESSES, &insert_processes_, "The processes in the annual cycle to be include in this initialisation phase", "", true);
  parameters_.Bind<string>(PARAM_EXCLUDE_PROCESSES, &exclude_processes_, "The processes in the annual cycle to be excluded from this initialisation phase", "", true);
  parameters_.Bind<unsigned>(PARAM_CONVERGENCE_YEARS, &convergence_years_, "The iteration (year) when the test for convergence (lambda) is evaluated", "", true)
      ->set_lower_bound(2, true);
  parameters_.Bind<Double>(PARAM_LAMBDA, &lambda_,
                           "The maximum value of the absolute sum of differences (lambda) between the partition at year-1 and year that indicates successful convergence", "",
                           Double(1e-10));
  parameters_.Bind<bool>(PARAM_PLUS_GROUP, &plus_group_, "Indicates if the convergence check applies only to the plus_group", "", false);
}

/**
 * Validate
 */
void Iterative::DoValidate() {
  for (string insert : insert_processes_) {
    vector<string> pieces;
    boost::split(pieces, insert, boost::is_any_of("()="), boost::token_compress_on);
    if (pieces.size() != 2 && pieces.size() != 3)
      LOG_ERROR_P(PARAM_INSERT_PROCESSES) << " value " << insert << " does not match the format time_step(process)=new_process = " << pieces.size();
  }
}

/**
 * Build
 */
void Iterative::DoBuild() {
  LOG_TRACE();
  time_steps_ = model_->managers()->time_step()->ordered_time_steps();

  // handle any new processes we want to insert
  for (string insert : insert_processes_) {
    vector<string> pieces;
    boost::split(pieces, insert, boost::is_any_of("()="), boost::token_compress_on);

    string target_process = pieces.size() == 3 ? pieces[1] : "";
    string new_process    = pieces.size() == 3 ? pieces[2] : pieces[1];

    auto time_step = model_->managers()->time_step()->GetTimeStep(pieces[0]);
    LOG_FINE() << "inserting " << new_process << " into time-step " << pieces[0] << " before the process " << target_process;
    vector<string> process_labels = time_step->initialisation_process_labels(label_);

    if (target_process == "") {
      process_labels.push_back(new_process);
    } else {
      vector<string>::iterator iter = std::find(process_labels.begin(), process_labels.end(), target_process);
      if (iter == process_labels.end())
        LOG_ERROR_P(PARAM_INSERT_PROCESSES) << " process " << target_process << " does not exist in time step " << time_step->label();
      process_labels.insert(iter, new_process);
    }

    time_step->SetInitialisationProcessLabels(label_, process_labels);
  }

  // handle the excludes we've specified
  for (string exclude : exclude_processes_) {
    unsigned count = 0;
    for (auto time_step : time_steps_) {
      vector<string> process_labels = time_step->initialisation_process_labels(label_);
      unsigned       size_before    = process_labels.size();
      process_labels.erase(std::remove_if(process_labels.begin(), process_labels.end(), [exclude](string& ex) { return exclude == ex; }), process_labels.end());
      unsigned diff = size_before - process_labels.size();

      time_step->SetInitialisationProcessLabels(label_, process_labels);
      count += diff;
    }

    if (count == 0)
      LOG_ERROR_P(PARAM_EXCLUDE_PROCESSES) << " process " << exclude << " does not exist in any time steps to be excluded.";
  }

  // ensure a convergence test is carried out, only keep valid convergence years, and remove duplicates
  if (convergence_years_.size() != 0) {
    if (std::find(convergence_years_.begin(), convergence_years_.end(), years_) == convergence_years_.end()) {
      convergence_years_.push_back(years_);
    }
  } else {
    convergence_years_.push_back(years_);
  }
  std::sort(convergence_years_.begin(), convergence_years_.end());
  vector<unsigned> valid_years;
  for (auto year : convergence_years_) {
    if (year <= years_) {
      if (std::find(valid_years.begin(), valid_years.end(), year) == valid_years.end()) {
        valid_years.push_back(year);
      }
    }
  }
  convergence_years_ = valid_years;

  // Build our partition
  vector<string> categories = model_->categories()->category_names();
  partition_.Init(categories);
  cached_partition_.Init(categories);

  // Find any BH_recruitment process in the annual cycle
  unsigned i = 0;
  for (auto time_step : model_->managers()->time_step()->ordered_time_steps()) {
    for (auto process : time_step->processes()) {
      if (process->process_type() == ProcessType::kRecruitment && process->type() == PARAM_RECRUITMENT_BEVERTON_HOLT) {
        LOG_FINEST() << "Found a BevertonHolt process";
        recruitment_process_.push_back(dynamic_cast<RecruitmentBevertonHolt*>(process));
        if (!recruitment_process_[i])
          LOG_CODE_ERROR() << "BevertonHolt Recruitment exists but dynamic cast pointer cannot be made, if (!recruitment) ";
        i++;
      } else if (process->process_type() == ProcessType::kRecruitment && process->type() == PARAM_RECRUITMENT_BEVERTON_HOLT_WITH_DEVIATIONS) {
        LOG_FINEST() << "Found a BevertonHolt process";
        recruitment_process_with_devs_.push_back(dynamic_cast<RecruitmentBevertonHoltWithDeviations*>(process));
        if (!recruitment_process_with_devs_[i])
          LOG_CODE_ERROR() << "BevertonHolt Recruitment with deviations exists but dynamic cast pointer cannot be made, if (!recruitment) ";
        i++;
      }
    }
  }
}

/**
 * Execute the iterative initialisation phases
 */
void Iterative::DoExecute() {
  LOG_TRACE();

  timesteps::Manager& time_step_manager = *model_->managers()->time_step();
  if (convergence_years_.size() == 0) {
    LOG_CODE_ERROR() << "In iterative convergence, convergence years have not been defined. This should have been done in code if it was not supplied by the user";
  } else {
    unsigned total_years   = 0;
    unsigned counter_years = 0;

    test_convergence_years_.resize(0);
    test_convergence_lambda_.resize(0);

    for (unsigned year : convergence_years_) {
      counter_years = year;
      time_step_manager.ExecuteInitialisation(label_, year - (total_years + 1));

      cached_partition_.BuildCache();
      time_step_manager.ExecuteInitialisation(label_, 1);
      total_years += year - (total_years + 1);
      if ((total_years + 1) >= years_) {
        // Have run for the maximum years_ defined
        CheckConvergence(year);
        break;
      } else {
        // Check if convergence obtained - if so, break
        if (CheckConvergence(year))
          break;
      }
      ++total_years;
      LOG_FINE() << "Initial year = " << year;
    }
    LOG_FINE() << label_ << " ran for '" << counter_years << "' years.";
  }

  LOG_FINE() << "Number of Beverton-Holt recruitment processes in annual cycle = " << recruitment_process_.size();
  LOG_FINE() << "Number of Beverton-Holt recruitment processes with deviations in annual cycle = " << recruitment_process_with_devs_.size() << " and partition has not been scaled";

  // We are at Equilibrium state here
  // Check if we have B0 initialised or R0 initialised recruitment
  bool B0_initial_recruitment = false;
  for (auto recruitment_process : recruitment_process_) {
    if (recruitment_process->b0_initialised() & !recruitment_process->has_partition_been_scaled()) {
      LOG_FINE() << PARAM_B0 << " has been defined for process labelled " << recruitment_process->label();
      recruitment_process->ScalePartition();
      B0_initial_recruitment = true;
    }
  }
  for (auto recruitment_process_with_devs : recruitment_process_with_devs_) {
    if (recruitment_process_with_devs->b0_initialised() & !recruitment_process_with_devs->has_partition_been_scaled()) {
      LOG_FINE() << PARAM_B0 << " has been defined for process labelled " << recruitment_process_with_devs->label() << " and partition has not been scaled";
      recruitment_process_with_devs->ScalePartition();
      B0_initial_recruitment = true;
    }
  }
  if (B0_initial_recruitment) {
    // Calculate derived quantities in the right space if we have a B0 initialised model
    timesteps::Manager& time_step_manager = *model_->managers()->time_step();
    time_step_manager.ExecuteInitialisation(label_, 1);
  }
}

/**
 * Check for convergence on the partition and return true if it less than the
 * lambda threshold to quit early and save time
 *
 * @return True if convergence, false otherwise
 */
bool Iterative::CheckConvergence(unsigned year) {
  LOG_TRACE();
  Double variance        = 0.0;
  Double sum             = 0.0;
  Double previous_sum    = 0.0;
  auto   cached_category = cached_partition_.begin();
  auto   category        = partition_.begin();

  for (; category != partition_.end(); ++cached_category, ++category) {
    if (plus_group_) {
      sum += (*category)->data_[(*category)->data_.size() - 1];
      previous_sum += cached_category->data_[cached_category->data_.size() - 1];
    } else {
      for (int i = 0; i < (int)(*category)->data_.size(); ++i) {
        sum += (*category)->data_[i];
        previous_sum += cached_category->data_[i];
      }
    }
  }
  LOG_FINE() << label_ << " had check values " << sum << " and " << previous_sum;
  if (sum == 0.0)
    variance = INFINITY;
  else
    variance = fabs(previous_sum - sum) / sum;

  test_convergence_years_.push_back(year);
  test_convergence_lambda_.push_back(variance);

  if (variance <= lambda_)
    return true;

  return false;
}

} /* namespace age */
} /* namespace initialisationphases */
} /* namespace niwa */
