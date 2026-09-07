"""Shared syntax for Kai test and documentation fixture directives."""
import json


def directive(source, name, required=False):
    prefix = f'// {name}:'
    values = [line[len(prefix):].strip() for line in source.splitlines()
              if line.startswith(prefix)]
    if not values and not required:
        return None
    if len(values) != 1 or not values[0]:
        raise ValueError(f'Missing, empty, or duplicate {name} fixture')
    return values[0]


def string_fixture(source, name, default=''):
    encoded = directive(source, name)
    if encoded is None:
        return default
    value = json.loads(encoded)
    if not isinstance(value, str):
        raise ValueError(f'{name} fixture must be a JSON string')
    value.encode('utf-8')  # Reject unpaired surrogates instead of failing during execution.
    return value


def validate_fixture(source, require_expect=True):
    directive(source, 'expect', required=require_expect)
    directive(source, 'expect-type')
    string_fixture(source, 'stdin')
    string_fixture(source, 'stdout')
