
package com.esotericsoftware.jsonbeans;

public interface JsonSerializable {
	public void write (com.esotericsoftware.jsonbeans.Json json);

	public void read (com.esotericsoftware.jsonbeans.Json json, JsonValue jsonData);
}
