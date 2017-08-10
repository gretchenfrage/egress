
package com.esotericsoftware.jsonbeans;

public interface JsonSerializer<T> {
	public void write (com.esotericsoftware.jsonbeans.Json json, T object, Class knownType);

	public T read (com.esotericsoftware.jsonbeans.Json json, JsonValue jsonData, Class type);
}
