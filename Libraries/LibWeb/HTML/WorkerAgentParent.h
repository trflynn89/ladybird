/*
 * Copyright (c) 2023, Andrew Kaster <akaster@serenityos.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <LibWeb/Bindings/AgentType.h>
#include <LibWeb/Bindings/RequestPrototype.h>
#include <LibWeb/Bindings/WorkerPrototype.h>
#include <LibWeb/Forward.h>
#include <LibWeb/Worker/WebWorkerClient.h>

namespace Web::HTML {

struct WorkerOptions {
    Bindings::WorkerType type { Bindings::WorkerType::Classic };
    Bindings::RequestCredentials credentials { Bindings::RequestCredentials::SameOrigin };
    String name { String {} };
};

// FIXME: Figure out a better naming convention for this type of parent/child process pattern.
class WorkerAgentParent : public JS::Cell {
    GC_CELL(WorkerAgentParent, JS::Cell);
    GC_DECLARE_ALLOCATOR(WorkerAgentParent);

protected:
    WorkerAgentParent(URL::URL url, WorkerOptions const& options, GC::Ptr<MessagePort> outside_port, GC::Ref<EnvironmentSettingsObject> outside_settings, Bindings::AgentType);
    virtual void initialize(JS::Realm&) override;
    virtual void visit_edges(Cell::Visitor&) override;

private:
    WorkerOptions m_worker_options;
    Bindings::AgentType m_agent_type { Bindings::AgentType::DedicatedWorker };
    URL::URL m_url;

    GC::Ptr<MessagePort> m_message_port;
    GC::Ptr<MessagePort> m_outside_port;
    GC::Ref<EnvironmentSettingsObject> m_outside_settings;

    RefPtr<Web::HTML::WebWorkerClient> m_worker_ipc;
};

}
