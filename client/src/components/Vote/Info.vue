<template>
	<div class="info white">
		<h2>Info</h2>
		<table>
			<tr>
				<th>ID:</th>
				<td>{{ id }}</td>
			</tr>
			<tr>
				<th>Name:</th>
				<td>{{ name }}</td>
			</tr>
			<tr>
				<th>Node:</th>
				<td>{{ bc }}</td>
			</tr>
			<tr>
				<th>SmartContract:</th>
				<td>{{ sc }}</td>
			</tr>
			<tr>
				<th>ABI:</th>
				<td>{{ abi }}</td>
			</tr>
		</table>
	</div>
</template>

<script>
export default {
	name: 'Info',
	props: {
		id: String,
		name: String,
		bc: String,
		sc: String,
		abi: String
	}
};
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
.info {
	height: 200px;
}

.info table {
	width: 100%;
}

th, td {
	text-align: left;
}
</style>
