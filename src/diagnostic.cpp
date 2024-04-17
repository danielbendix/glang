#include "diagnostic.h"

NoopWriter noop;

DiagnosticWriter *Diagnostic::current = &noop;
