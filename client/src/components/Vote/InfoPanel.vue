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
				<td><div>{{ name }}</div></td>
			</tr>
			<tr>
				<th>Node:</th>
				<td><div>{{ bc }}</div></td>
			</tr>
			<tr>
				<th>SmartContract:</th>
				<td><div>{{ sc }}</div></td>
			</tr>
			<tr>
				<th>ABI:</th>
				<td><div>{{ abi }}</div></td>
			</tr>
		</table>
	</div>
</template>

<script>
export default {
	name: 'InfoPanel',
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
	min-height: 200px;
}

.info table {
	width: 100%;
}

th, td {
	text-align: left;
}
</style>
