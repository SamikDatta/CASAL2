/**
 * @file TimeStep.h
 * @author  Scott Rasmussen (scott.rasmussen@zaita.com)
 * @version 1.0
 * @date 13/12/2012
 * @section LICENSE
 *
 * Copyright NIWA Science �2012 - www.niwa.co.nz
 *
 * @section DESCRIPTION
 *
 * The time class represents a moment of time.
 *
 * $Date: 2008-03-04 16:33:32 +1300 (Tue, 04 Mar 2008) $
 */
#ifndef TIMESTEP_H_
#define TIMESTEP_H_

// Headers
#include "BaseClasses/Object.h"
#include "BaseClasses/Executor.h"
#include "DerivedQuantities/Manager.h"
#include "Processes/Process.h"

// Namespaces
namespace isam {

/**
 * Class Definition
 */
class TimeStep : public isam::base::Object {
public:
  // Methods
  TimeStep();
  virtual                     ~TimeStep() = default;
  void                        Validate();
  void                        Build();
  void                        Reset() {};
  void                        ExecuteForInitialisation(const string& phase_label);
  void                        Execute(unsigned year);
  bool                        HasProcess(const string& label) { return std::find(process_names_.begin(), process_names_.end(), label) != process_names_.end(); }
  void                        Subscribe(ExecutorPtr executor, unsigned year) { executors_[year].push_back(executor); }
  void                        SubscribeToInitialisationBlock(ExecutorPtr executor) { initialisation_block_executors_.push_back(executor); }
  void                        SubscribeToBlock(ExecutorPtr executor);
  void                        SubscribeToBlock(ExecutorPtr executor, unsigned year) { block_executors_[year].push_back(executor); }
  void                        SetInitialisationProcessLabels(const string& initialisation_phase_label, vector<string> process_labels_);
  void                        BuildInitialisationProcesses();

  // accessors
  vector<string>              process_labels() const { return process_names_; }
  vector<string>              initialisation_process_labels(const string& initialisation_phase) { return initialisation_process_labels_[initialisation_phase]; }

private:
  // methods
//  void                        ExecuteDerivedQuantities();
//  void                        ExecuteInitialisationDerivedQuantities(unsigned phase);

  // Members
  vector<string>                      process_names_;
  vector<ProcessPtr>                  processes_;
  map<unsigned, vector<ExecutorPtr> > executors_;
  map<unsigned, vector<ExecutorPtr> > block_executors_;
  vector<ExecutorPtr>                 initialisation_block_executors_;
  unsigned                            block_start_process_index_;
  unsigned                            block_end_process_Index_;
  map<string, vector<string>>         initialisation_process_labels_;
  map<string, vector<ProcessPtr>>     initialisation_processes_;
};

/**
 * Typedef
 */
typedef boost::shared_ptr<TimeStep> TimeStepPtr;

} /* namespace isam */
#endif /* TIMESTEP_H_ */
