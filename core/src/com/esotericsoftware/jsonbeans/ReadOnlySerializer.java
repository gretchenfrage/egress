
package com.esotericsoftware.jsonbeans;

public abstract class ReadOnlySerializer<T> implements com.esotericsoftware.jsonbeans.JsonSerializer<T> {
	public void write (com.esotericsoftware.jsonbeans.Json json, T object, Class knownType) {
	}

	abstract public T read (Json json, JsonValue jsonData, Class type);
}
