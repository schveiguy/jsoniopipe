module iopipe.json.common;

/// release policy for the input stream for use when serializing DOM or data.
enum ReleasePolicy
{
    never, /// don't release ever.
    afterMembers /// release after parsing members of arrays or structs.
}
